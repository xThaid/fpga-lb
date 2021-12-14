#include "memctrl.h"

#include <cstdio>

IBusCtrl::IBusCtrl(Memory* mem) :
    avalonDataValid(0),
    avalonData(0),
    avalonWaitRequestn(1),
    avalonResponse(0),
    mem(mem) {

    inSig(&avalonAddress);
    inSig(&avalonRead);
}

void IBusCtrl::reset() {
}

void IBusCtrl::cycle() {
    if (avalonRead.get() && avalonWaitRequestn.get()) {
        printf("[IBUS] Got request address: %08x\n", avalonAddress.get());
        IBusAvalonRsp rsp;
        mem->read(avalonAddress.get(), 4, (uint8_t*)&rsp.data);
        rsp.error = false;
        respQ.push(rsp);
    }

    if (!respQ.empty()) {
        IBusAvalonRsp rsp = respQ.front(); respQ.pop();
        avalonData.set(rsp.data);
        avalonDataValid.set(1);
        avalonResponse.set(rsp.error ? 3 : 0);
    } else {
        avalonDataValid.set(0);
    }
}

void IBusCtrl::postCycle() {

}

DBusCtrl::DBusCtrl(Memory* mem) :
    avalonDataValid(0),
    avalonData(0),
    avalonWaitRequestn(1),
    avalonResponse(0),
    mem(mem) {

    inSig(&avalonAddress);
    inSig(&avalonRead);
    inSig(&avalonByteEnable);
    inSig(&avalonWrite);
    inSig(&avalonWriteData);
}

void DBusCtrl::reset() {
}

void DBusCtrl::cycle() {
    if (avalonWrite.get() && avalonWaitRequestn.get()) {
        printf("[DBUS] Got write request address: %08x, byteEnable: %01x, writeData: %08x\n", avalonAddress.get(), avalonByteEnable.get(), avalonWriteData.get());
        uint32_t writeData = avalonWriteData.get();
        mem->write(avalonAddress.get(), avalonByteEnable.get(), (uint8_t*)&writeData);
    }

    if (avalonRead.get() && avalonWaitRequestn.get()) {
        DBusAvalonRsp rsp;
        mem->read(avalonAddress.get(), 4, (uint8_t*)&rsp.data);
        printf("[DBUS] Got read request address: %08x, data: %08x\n", avalonAddress.get(), rsp.data);
        rsp.error = false;
        respQ.push(rsp);
    }

    if (!respQ.empty()) {
        DBusAvalonRsp rsp = respQ.front(); respQ.pop();
        avalonData.set(rsp.data);
        avalonDataValid.set(1);
        avalonResponse.set(rsp.error ? 3 : 0);
    } else {
        avalonDataValid.set(0);
    }
}

void DBusCtrl::postCycle() {
    
}