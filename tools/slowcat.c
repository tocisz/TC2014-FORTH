#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define DEFAULT_DELAY 25000         // Default delay: 25 milliseconds (in microseconds)
#define DEFAULT_NEWLINE_DELAY 100000 // Default newline delay: 100 milliseconds (in microseconds)

// Function to print file contents with delays
void cat_with_delay(FILE *file, int delay, int newline_delay) {
    int ch;
    while ((ch = fgetc(file)) != EOF) {
        putchar(ch);
        fflush(stdout);       // Ensure each character is printed immediately

        // Apply delay based on character
        if (ch == '\n') {
            usleep(newline_delay);
        } else {
            usleep(delay);
        }
    }
}

int main(int argc, char *argv[]) {
    int delay = DEFAULT_DELAY;
    int newline_delay = DEFAULT_NEWLINE_DELAY;
    FILE *file;

    // Parse optional arguments for regular and newline delay
    if (argc > 1 && argv[1][0] == '-') {
        delay = atoi(argv[1] + 1) * 1000; // Convert milliseconds to microseconds
        argc--;
        argv++;
    }
    if (argc > 1 && argv[1][0] == '+') {
        newline_delay = atoi(argv[1] + 1) * 1000; // Convert milliseconds to microseconds
        argc--;
        argv++;
    }

    // If a filename is provided, open the file
    if (argc > 1) {
        file = fopen(argv[1], "r");
        if (file == NULL) {
            perror("Error opening file");
            return 1;
        }
    } else {
        file = stdin;
    }

    // Call the function with specified delays
    cat_with_delay(file, delay, newline_delay);

    // Close file if it was opened
    if (file != stdin) {
        fclose(file);
    }

    return 0;
}
