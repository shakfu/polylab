#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

// function prototypes
int mmio_read(const char* filename);
int mmio_write(const char* filename, const char* text);

int main()
{
    // You can change filename and text for experimentation
    const char* filename = "example.txt";
    const char* text = "Hello, mmap!";

    printf("[Step 1:] Reading a non-existing file via mmap. Should give an "
           "error.\n");
    printf("Error is printed on stderr (instead of stdout).\n");
    mmio_read(filename);

    printf("[Step 2:] Creating a new file and writing to it via mmap.\n");
    mmio_write(filename, text);

    printf("[Step 3:] Reading what we wrote in the previous step.\n");
    mmio_read(filename);

    return 0;
}


// writing using MMIO
int mmio_write(const char* filename, const char* text)
{

    // Open the file for writing
    //(mode_t)0600 means that the file will have read and write permissions
    // for the owner of the file (the "0600" represents octal notation for
    // permissions).
    int fd = open(filename, O_RDWR | O_CREAT | O_TRUNC, (mode_t)0600);
    if (fd == -1) {
        perror("File openning failed.");
        return EXIT_FAILURE;
    }

    // Determine the size of the file
    off_t file_size = strlen(text);

    // Set the file size using ftruncate
    // Writing to a file region via MMIO which does not actually exist will
    // generate a sigbus fault.
    if (ftruncate(fd, file_size) == -1) {
        close(fd);
        perror("ftruncate failed.");
        return EXIT_FAILURE;
    }

    // Map the file into memory
    char* mapped_data = (char*)mmap(NULL, file_size, PROT_READ | PROT_WRITE,
                                    MAP_SHARED, fd, 0);
    if (mapped_data == MAP_FAILED) {
        close(fd);
        perror("mmap");
        return EXIT_FAILURE;
    }

    // Copy the data into the mapped memory
    memcpy(mapped_data, text, file_size);

    // Flush changes to the file (optional)
    // msync has associated IO cost because data is forced to flush to
    // persistent store.
    if (msync(mapped_data, file_size, MS_SYNC) == -1) {
        perror("msync");
    }

    // Unmap the memory
    if (munmap(mapped_data, file_size) == -1) {
        perror("munmap");
    }

    // Close the file
    close(fd);

    printf("Data has been written to %s\n", filename);
}


// reading using MMIO
int mmio_read(const char* filename)
{
    // Open the file for reading
    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        perror("File opening failed.");
        return EXIT_FAILURE;
    }

    // Determine the size of the file
    struct stat file_info;
    if (fstat(fd, &file_info) == -1) {
        close(fd);
        perror("fstat failed.");
        return EXIT_FAILURE;
    }
    off_t file_size = file_info.st_size;

    // Map the file into memory for reading
    char* mapped_data = (char*)mmap(NULL, file_size, PROT_READ, MAP_PRIVATE,
                                    fd, 0);
    if (mapped_data == MAP_FAILED) {
        close(fd);
        perror("mmap");
        return EXIT_FAILURE;
    }

    // Close the file (not needed after mapping)
    close(fd);

    // Now you can access the contents of the file using mapped_data
    // For example, printing the file contents:
    printf("File Contents:\n%s\n", mapped_data);

    // Unmap the memory
    if (munmap(mapped_data, file_size) == -1) {
        perror("munmap");
    }
}
