/* ppc-darwin-mkrospace.c - write a .o which makes sure we get our desired address range */

#include <stdlib.h>
#include <string.h>
#include <mach-o/loader.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>

#include "sbcl.h"
#include "runtime.h"

#include "ppc-darwin-spacelist.h"

struct simple_object_file
{
  struct segment_command thesegment;
  struct section thesection;
};

int main(int argc, char** argv)
{
  struct mach_header *theheader;
  struct simple_object_file *theobj;
  int fd, i;

  /* Produce the mach header */
  theheader = (struct mach_header*) malloc(sizeof(struct mach_header));
  theheader->magic = MH_MAGIC;
  theheader->cputype = CPU_TYPE_POWERPC;
  theheader->cpusubtype = CPU_SUBTYPE_POWERPC_ALL;
  theheader->filetype = MH_OBJECT;
  theheader->ncmds = N_SEGMENTS_TO_PRODUCE;
  theheader->sizeofcmds = N_SEGMENTS_TO_PRODUCE * (sizeof(struct segment_command) + sizeof(struct section));
  theheader->flags = MH_NOUNDEFS;
  printf("-Wl");
  fd = open("ppc-darwin-rospace.o", O_WRONLY | O_CREAT, 0644);
  write(fd, theheader, sizeof(struct mach_header));

  for (i = 0; i < N_SEGMENTS_TO_PRODUCE; i++) {
    theobj = (struct simple_object_file*) malloc(sizeof(struct simple_object_file));
    theobj->thesegment.cmd = LC_SEGMENT;
    theobj->thesegment.cmdsize = sizeof(struct segment_command) + sizeof(struct section);
    snprintf(theobj->thesegment.segname, 7, "SBCL%d", i);
    theobj->thesegment.fileoff = sizeof(struct mach_header) + i * (sizeof(struct segment_command) + sizeof(struct section));
    theobj->thesegment.vmaddr = space_start_locations[i];
    theobj->thesegment.vmsize = 0;
    theobj->thesegment.maxprot = VM_PROT_ALL;
    theobj->thesegment.initprot = VM_PROT_ALL;
    theobj->thesegment.nsects = 1;
    theobj->thesegment.flags = 0;
    snprintf(theobj->thesection.sectname, 7, "sect%d", i);
    snprintf(theobj->thesection.segname, 7, "SBCL%d", i);
    theobj->thesection.addr = space_start_locations[i];
    theobj->thesection.size = 0;
    theobj->thesection.offset = 0;
    theobj->thesection.align = 0;
    theobj->thesection.reloff = 0;
    theobj->thesection.nreloc = 0;
    theobj->thesection.flags = S_ZEROFILL | S_REGULAR | S_ATTR_SOME_INSTRUCTIONS;
    theobj->thesection.reserved1 = 0;
    theobj->thesection.reserved2 = 0;
    write(fd, theobj, sizeof(struct simple_object_file));
    printf(",-segaddr,SBCL%d,0x%x", i, space_start_locations[i]);
    free(theobj);
  }
  printf("\n");
  close(fd);
  return 0;
}
