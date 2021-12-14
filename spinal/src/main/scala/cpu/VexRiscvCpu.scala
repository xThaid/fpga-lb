

package cpu

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.eda.altera.{InterruptReceiverTag, QSysify, ResetEmitterTag}
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMConfig}

import vexriscv.plugin._
import vexriscv.{plugin, VexRiscv, VexRiscvConfig}

case class VexRiscvCpu() extends Component
{
    val io = new Bundle {
        val iBus                    = master(AvalonMM(IBusSimpleBus.getAvalonConfig()))
        val dBus                    = master(AvalonMM(DBusSimpleBus.getAvalonConfig()))
        val timerInterrupt          = in(Bool)
        val externalInterrupt       = in(Bool)
        val jtag                    = slave(Jtag())
    }

    // Configure a reasonable performance VexRiscv with compressed instructions.
    // Different kinds of exceptions are enabled  (illegal access, address
    // misalignment) because it makes sense to do that when you have a debugger
    // connected...

    val config = VexRiscvConfig(
        plugins = List(
            new IBusSimplePlugin(
                resetVector             = 0x00000000l,
                cmdForkOnSecondStage    = true,
                cmdForkPersistence      = true,
                prediction              = STATIC,
                // Trap when an iBus access returns an error.
                catchAccessFault        = true,    
                compressedGen           = true
            ),
            new DBusSimplePlugin(
                // Trap when a load or store access is misaligned.
                catchAddressMisaligned  = true,     
                // Trap when a load or store access results in a bus error
                catchAccessFault        = true
            ),
            new CsrPlugin(
                CsrPluginConfig(
                    // Trap when accessing a priviledged CSR from non-priviledged mode. Or when
                    // there are MRET or SRET instructions during non-priviledged mode. And other CSR
                    // related errors.
                    catchIllegalAccess  = true,
                    mvendorid           = null,
                    marchid             = null,
                    mimpid              = null,
                    mhartid             = null,
                    misaExtensionsInit  = 66,
                    misaAccess          = CsrAccess.NONE,
                    mtvecAccess         = CsrAccess.NONE,
                    mtvecInit           = 0x00000020,
                    mepcAccess          = CsrAccess.READ_WRITE,
                    mscratchGen         = false,
                    mcauseAccess        = CsrAccess.READ_ONLY,
                    mbadaddrAccess      = CsrAccess.READ_ONLY,    // == mtvalAccess
                    mcycleAccess        = CsrAccess.NONE,
                    minstretAccess      = CsrAccess.NONE,
                    ecallGen            = true,
                    ebreakGen           = true,
                    wfiGenAsWait        = false,
                    ucycleAccess        = CsrAccess.READ_ONLY,
                    uinstretAccess      = CsrAccess.NONE
                )
            ),
            new DecoderSimplePlugin(
                // Trap when we execute an illegal instruction. For example, a MUL instruction when
                // the CPU hasn't been configured with HW multiplier support.
                catchIllegalInstruction = true
            ),
            new RegFilePlugin(
                regFileReadyKind        = plugin.SYNC,
                zeroBoot                = false
            ),
            new IntAluPlugin,
            new SrcPlugin(
                separatedAddSub         = false,
                executeInsertion        = false
            ),
            new FullBarrelShifterPlugin,
            new HazardSimplePlugin(
                bypassExecute           = true,
                bypassMemory            = true,
                bypassWriteBack         = true,
                bypassWriteBackBuffer   = true,
                pessimisticUseSrc       = false,
                pessimisticWriteRegFile = false,
                pessimisticAddressMatch = false
            ),
            new BranchPlugin(
                earlyBranch             = false,
                catchAddressMisaligned  = true
            ),
            new DebugPlugin(ClockDomain.current),
            new YamlPlugin("cpu0.yaml")
        )
    )

    // Instantiate the CPU
    val cpu = new VexRiscv(config)

    // Map the busses of the cpu to external IO ports of this module.
    for(plugin <- cpu.plugins) plugin match{
        case plugin : IBusSimplePlugin  =>  {
                                                io.iBus <> plugin.iBus.toAvalon()
                                                io.iBus.setName("iBusAvalon")
                                                io.iBus.addTag(ClockDomainTag(ClockDomain.current))
                                            }
        case plugin : DBusSimplePlugin  =>  {
                                                io.dBus <> plugin.dBus.toAvalon()
                                                io.dBus.setName("dBusAvalon")
                                                io.dBus.addTag(ClockDomainTag(ClockDomain.current))
                                            }
        case plugin : CsrPlugin     => { 
                                            io.timerInterrupt     <> plugin.timerInterrupt
                                            io.externalInterrupt  <> plugin.externalInterrupt
                                       }
        case plugin : DebugPlugin   => plugin.debugClockDomain { 
                                            io.jtag               <> plugin.io.bus.fromJtag() 
                                       }
        case _ =>
    }
    
}

object VexRiscvCpu {
    def main(args: Array[String]) {
        val report = SpinalVerilog{
            val toplevel = new VexRiscvCpu()
            toplevel
        }
        QSysify(report.toplevel)
    }
}

