#include "pico/stdlib.h"
#include "hardware/uart.h"
#include "hardware/irq.h"

#define UART_ID uart0
#define BAUD_RATE 19200
#define DATA_BITS 8
#define STOP_BITS 1
#define PARITY UART_PARITY_NONE

// We are using pins 0 and 1, but see the GPIO function select table in the
// datasheet for information on which other pins can be used.
#define UART_TX_PIN 0
#define UART_RX_PIN 1

// Buffer for received lines

#define LINE_LENGTH 256
static char lines[2][LINE_LENGTH] = { {0}, {0} };
static volatile int current_line = 1; // Current line to read from
static int current_index = 0;
static int working_line = 0;
static int working_index = 0;

// RX interrupt handler
void on_uart_rx()
{
    while (uart_is_readable(UART_ID))
    {
        uint8_t ch = uart_getc(UART_ID);
        // Can we send it back?
        if (uart_is_writable(UART_ID))
        {
            if (ch == 0x08 || ch == 0x7F)
            { // Handle backspace
                if (working_index > 0)
                {
                    working_index--;
                    uart_putc(UART_ID, '\b');                   // Send backspace
                    uart_putc(UART_ID, ' ');                    // Overwrite with space
                    uart_putc(UART_ID, '\b');                   // Move back again
                }
                else
                {
                    uart_putc(UART_ID, '\a');                   // Beep if trying to backspace at start
                }
            }
            else if (ch == 0x0D || ch == 0x0A)
            {                                                   // Handle new line
                lines[working_line][working_index] = '\0';      // Null-terminate the string
                current_line = working_line;                    // Set current line to the one we just finished
                working_line = working_line ? 0 : 1;            // Swap around
                working_index = 0;                              // Reset for next line
                lines[working_line][working_index] = '\0';      // Prepare next line
                uart_putc(UART_ID, '\x0D');                       // Echo newline
                //uart_puts(UART_ID, " ok\n");                       // Echo newline
            }
            else if (ch >= 0x20 && ch<0x7F && working_index < LINE_LENGTH - 1)
            {                                                   // Avoid overflow
                lines[working_line][working_index++] = ch;      // Store character in the line buffer
                uart_putc(UART_ID, ch);                         // Echo the character
            }
            else
            {
                uart_putc(UART_ID, '\a');                       // Beep if line is full or illegal character
            }
        }
    }
}

// Read the next character from the current line
// Blocks until a character is available
char _key()
{
    while (1)
    {
        // Check if we have a line to read
        while (lines[current_line][0] == '\0')
        {
            tight_loop_contents(); // Wait for a line
        }

        // Read the next character from the current line
        char ch = lines[current_line][current_index++];
        if (ch == '\0')
        {
            current_index = 0; // Reset index for next read
            lines[current_line][0] = '\0'; // Clear the line
            return 0x0D; // End of line
        }
        else
        {
            return ch;
        }
    }
}

// Write a character to the UART
void _emit(char ch)
{
    while (!uart_is_writable(UART_ID))
    {
        tight_loop_contents(); // Wait until we can write
    }
    uart_putc(UART_ID, ch); // Send the character
}

void _emit_string(const char *str, int len)
{
    if ( len > 255 )
    {
        len = 255; // Limit to 255 characters
    }
    while (len--)
    {
        _emit(*str++);
    }
}

int sdk_init()
{
    // Set up our UART with a basic baud rate.
    uart_init(UART_ID, 2400);

    // Set the TX and RX pins by using the function select on the GPIO
    // Set datasheet for more information on function select
    gpio_set_function(UART_TX_PIN, UART_FUNCSEL_NUM(UART_ID, UART_TX_PIN));
    gpio_set_function(UART_RX_PIN, UART_FUNCSEL_NUM(UART_ID, UART_RX_PIN));

    // Actually, we want a different speed
    // The call will return the actual baud rate selected, which will be as close as
    // possible to that requested
    int __unused actual = uart_set_baudrate(UART_ID, BAUD_RATE);

    // Set UART flow control CTS/RTS, we don't want these, so turn them off
    uart_set_hw_flow(UART_ID, false, false);

    // Set our data format
    uart_set_format(UART_ID, DATA_BITS, STOP_BITS, PARITY);

    // Turn off FIFO's - we want to do this character by character
    uart_set_fifo_enabled(UART_ID, false);

    // Set up a RX interrupt
    // We need to set up the handler first
    // Select correct interrupt for the UART we are using
    int UART_IRQ = UART_ID == uart0 ? UART0_IRQ : UART1_IRQ;

    // And set up and enable the interrupt handlers
    irq_set_exclusive_handler(UART_IRQ, on_uart_rx);
    irq_set_enabled(UART_IRQ, true);

    // Now enable the UART to send interrupts - RX only
    uart_set_irq_enables(UART_ID, true, false);

    // OK, all set up.
    // Lets send a basic string out, and then run a loop and wait for RX interrupts
    // The handler will count them, but also reflect the incoming data back with a slight change!
    uart_puts(UART_ID, "\x0DLeducForth\x0D");
}