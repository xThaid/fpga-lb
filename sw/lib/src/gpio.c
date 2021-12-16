#include "gpio.h"

#include "io.h"

int gpio_led_read(int led) {
    return (GPIO.LED & led) != 0;
}

void gpio_led_write(int led, int val) {
    if (val)
        GPIO.LED |= led;
    else
        GPIO.LED &= ~led;
}

void gpio_led_toggle(int led) {
    GPIO.LED ^= led;
}
