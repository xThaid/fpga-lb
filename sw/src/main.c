#include "FreeRTOS.h"
#include "task.h"
#include "semphr.h"

#include "io.h"
#include "vsscanf.h"

#include "config.h"

void display_num(uint32_t number) {
    int digits[8] = {-1, -1, -1, -1, -1, -1, -1, 0};
    int digit = 7;

    while (number > 0 && digit >= 0) {
        digits[digit] = number % 10;
        digit--;
        number /= 10;
    }

    for (int i = 0; i < 8; i++)
        gpio_7seg_set_digit(i, digits[i]);
}

void stats_display_task(void *pvParameters) {
    while(1) {
        dataplane_stats_t total_stats = dataplane_get_stats(-1);
        display_num(total_stats.pkt_cnt);

        vTaskDelay(100);
    }
}

void stats_sender_task(void *pvParameters) {
    while(1) {
        for (int i = 0; i < MAX_REALS_CNT; i++) {
            dataplane_stats_t stats = dataplane_get_stats(i);
            jtag_uart_printf("%d %u %u %u\n", i, stats.pkt_cnt, (uint32_t) (stats.bytes_cnt >> 32), (uint32_t) stats.bytes_cnt);
        }

        load_generator_stats_t stats = load_generator_get_stats();
        jtag_uart_printf("100 %u %u %u\n", stats.pkt_cnt, (uint32_t) (stats.bytes_cnt >> 32), (uint32_t) stats.bytes_cnt);
        
        vTaskDelay(100);
    }
}

void command_reader_task(void *pvParameters) {
    char *buf = pvPortMalloc(80);

    while(1) {
        jtag_uart_readline(buf, 80);

        int lg_enabled, lg_tx_period, lg_payload_len;
        
        sscanf(buf, "%d %d %d", &lg_enabled, &lg_tx_period, &lg_payload_len);

        load_generator_set_payload_len(lg_payload_len);
        load_generator_set_tx_period(lg_tx_period);
        load_generator_set_enabled(lg_enabled);
    }
}

void heartBeatTask(void *pvParameters) {
    while(1) {
        gpio_led_toggle(GPIO_LEDG(8));

        vTaskDelay(500);
    }
}

static uint32_t build_ip(const uint8_t *arr) {
    return (((uint32_t) arr[0]) << 24) | (arr[1] << 16) | (arr[2] << 8) | (arr[3] << 0);
}

static void dataplane_setup() {
    dataplane_set_mac_addr(MAC0_ADDR);

    for (int i = 0; i < MAX_VIPS_CNT; i++) {
        if (VIPS[i][0] == 0)
            continue;

        dataplane_add_vip(build_ip(VIPS[i]), i);
        dataplane_update_hashring(i, HASHRING[i]);
    }

    for (int i = 0; i < MAX_REALS_CNT; i++) {
        if (REALS[i][0] == 0)
            continue;

        dataplane_update_real(i, build_ip(REALS[i]));
    }
}

static void load_generator_setup(void) {
    load_generator_init(LG_MAC_ADDR, build_ip(LG_SRC_IP), build_ip(LG_DST_IP), LG_SRC_PORT, LG_DST_PORT);
    load_generator_set_payload_len(16);
    load_generator_set_tx_period(50000000);
}

int main(void) {
    dataplane_setup();
    load_generator_setup();
    tse0_setup(MAC0_ADDR);
    tse1_setup(MAC1_ADDR);

    xTaskCreate(heartBeatTask, "Heartbeat task", 128, NULL, 1, NULL);
    xTaskCreate(stats_display_task, "Stats display task", 256, NULL, 1, NULL);
    xTaskCreate(stats_sender_task, "Stats sender task", 256, NULL, 1, NULL);
    xTaskCreate(command_reader_task, "Command reader task", 256, NULL, 1, NULL);

    vTaskStartScheduler();

    return 0;
}

void vApplicationMallocFailedHook( void )
{
    /* vApplicationMallocFailedHook() will only be called if
    configUSE_MALLOC_FAILED_HOOK is set to 1 in FreeRTOSConfig.h.  It is a hook
    function that will get called if a call to pvPortMalloc() fails.
    pvPortMalloc() is called internally by the kernel whenever a task, queue,
    timer or semaphore is created.  It is also called by various parts of the
    demo application.  If heap_1.c or heap_2.c are used, then the size of the
    heap available to pvPortMalloc() is defined by configTOTAL_HEAP_SIZE in
    FreeRTOSConfig.h, and the xPortGetFreeHeapSize() API function can be used
    to query the size of free heap space that remains (although it does not
    provide information on how the remaining heap might be fragmented). */
    taskDISABLE_INTERRUPTS();
    gpio_led_write(GPIO_LEDR(17), 1);
    for( ;; );
}
/*-----------------------------------------------------------*/

void vApplicationIdleHook( void )
{
    /* vApplicationIdleHook() will only be called if configUSE_IDLE_HOOK is set
    to 1 in FreeRTOSConfig.h.  It will be called on each iteration of the idle
    task.  It is essential that code added to this hook function never attempts
    to block in any way (for example, call xQueueReceive() with a block time
    specified, or call vTaskDelay()).  If the application makes use of the
    vTaskDelete() API function (as this demo application does) then it is also
    important that vApplicationIdleHook() is permitted to return to its calling
    function, because it is the responsibility of the idle task to clean up
    memory allocated by the kernel to any task that has since been deleted. */
}
/*-----------------------------------------------------------*/

void vApplicationStackOverflowHook( TaskHandle_t pxTask, char *pcTaskName )
{
    ( void ) pcTaskName;
    ( void ) pxTask;

    /* Run time stack overflow checking is performed if
    configCHECK_FOR_STACK_OVERFLOW is defined to 1 or 2.  This hook
    function is called if a stack overflow is detected. */
    taskDISABLE_INTERRUPTS();
    gpio_led_write(GPIO_LEDR(16), 1);
    for( ;; );
}
/*-----------------------------------------------------------*/

void vApplicationTickHook( void )
{
}
/*-----------------------------------------------------------*/

void vAssertCalled( void )
{
    volatile uint32_t ulSetTo1ToExitFunction = 0;

    taskDISABLE_INTERRUPTS();
    gpio_led_write(GPIO_LEDR(15), 1);
    while( ulSetTo1ToExitFunction != 1 )
    {
        __asm volatile( "NOP" );
    }
}

void SystemIrqHandler(uint32_t mcause) 
{
    gpio_led_write(GPIO_LEDR(14), 1);
    //jtag_uart_printf("freeRTOS: Unknown interrupt (0x%x)\n", mcause);
}
