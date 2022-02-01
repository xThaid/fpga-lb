#ifndef LIB_GPIO_H
#define LIB_GPIO_H

int gpio_led_read(int led);
void gpio_led_write(int led, int val);
void gpio_led_toggle(int led);

void gpio_7seg_set_digit(int seg, int digit);

#endif