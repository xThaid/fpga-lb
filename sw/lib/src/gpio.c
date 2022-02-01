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

static uint32_t hex2seg[16] = {
    0b1000000,
    0b1111001,
    0b0100100,
    0b0110000,
    0b0011001,
    0b0010010,
    0b0000010,
    0b1111000,
    0b0000000,
    0b0010000
};

void gpio_7seg_set_digit(int seg, int digit) {
    if (digit == -1)
        GPIO.hex[seg] = 0b1111111;
    else
        GPIO.hex[seg] = hex2seg[digit];
}
