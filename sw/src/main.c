#include "FreeRTOS.h"
#include "task.h"
#include "semphr.h"

#include "io.h"

#define DELAY_LOOP 10000

int fibonacci[100];

void task1 (void *pvParameters) {
    fibonacci[0] = 0;
    fibonacci[1] = 1;

    for (int i = 2; i < 100; i++) {
        fibonacci[i] = fibonacci[i - 1] + fibonacci[i - 2];
    }

    char buf[32];

    while (1) {
        int n = jtag_uart_readline(buf, sizeof(buf));
        for (int i = 0; i < n / 2; i++) {
            char c = buf[i];
            buf[i] = buf[n - i - 1];
            buf[n - i - 1] = c;
        }

        jtag_uart_print("Reversed: ");
        jtag_uart_print(buf);
        jtag_uart_putc('\n');
    }
}

void task2 (void *pvParameters) {
    while(1) {
        jtag_uart_print("Hello there!\n");

        vTaskDelay(1000);
    }
}

int main(void)
{
    jtag_uart_print("Starting FreeRTOS\n");
    //xTaskCreate(task1, "Task 1", 100, NULL, 1, NULL);
    xTaskCreate(task2, "Task 2", 100, NULL, 1, NULL);
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
    while( ulSetTo1ToExitFunction != 1 )
    {
        __asm volatile( "NOP" );
    }
}

void SystemIrqHandler(uint32_t mcause) 
{
    //jtag_uart_printf("freeRTOS: Unknown interrupt (0x%x)\n", mcause);
}
