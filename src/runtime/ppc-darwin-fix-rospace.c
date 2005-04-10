/* ppc-darwin-fix-rospace.c - fix the segment and section output by ppc-darwin-mkrospace.c to have the correct size */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mach-o/loader.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>

#include "sbcl.h"
#include "runtime.h"

#include "ppc-darwin-spacelist.h"

int main(int argc, char** argv)
{
  int fd;
  int i, spacei;
  struct stat filestat;
  struct mach_header *header;
  void* file_pointer;
  size_t filesize;
  size_t old_commands_size;
  char* window;
  size_t segments_size;

  /* Open the SBCL binary */
  fd = open("sbcl", O_RDWR, 0); /* wr unx prgrmmrs, we cn't spll */
  if (fd == -1) {
    perror(argv[0]);
    return -1;
  }
  if (fstat(fd, &filestat) == -1) {
    close(fd);
    perror(argv[0]);
    return -1;
  }
  filesize = filestat.st_size;
  if ((int) (file_pointer = mmap(NULL, filesize, PROT_READ | PROT_WRITE, MAP_FILE | MAP_SHARED, fd, 0)) == -1) {
    perror(argv[0]);
    close(fd);
    return -1;
  }

  segments_size = 0;
  spacei = 0;
  header = (struct mach_header*) file_pointer;
  window = (char*) (header + 1);
  for (i = 0; i < header->ncmds; i++) {
    struct load_command* current_segment;
    current_segment = (struct load_command*) window;
    segments_size += current_segment->cmdsize;
    window += current_segment->cmdsize;
    if (current_segment->cmd == LC_SEGMENT) {
      int is_sbcl;
      struct segment_command* seg = (struct segment_command*) current_segment;
      struct section* sectptr;
      int j, max;
      max = seg->nsects;
      if (strncmp("SBCL", seg->segname, 4) == 0) {
	is_sbcl = 1;
	seg->vmsize = space_sizes[spacei];
      } else {
	is_sbcl = 0;
      }
      seg++;
      sectptr = (struct section*) seg;
      for (j = 0; j < max; j++) {
	if (is_sbcl) {
	  sectptr->size = space_sizes[spacei];
	  spacei++;
	}
	sectptr++;
      }
    }
  }

  munmap(file_pointer, filesize);
  close(fd);
  return 0;
}
