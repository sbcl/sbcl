/* ppc-darwin-mkrospace.c - write a .o which makes sure we get our desired address range */

#include <stdlib.h>
#include <string.h>
#include <mach-o/loader.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include "runtime.h"
#include "sbcl.h"

struct simple_object_file
{
  struct mach_header theheader;
  struct segment_command thesegment;
  struct section thesection;
};

int main(int argc, char** argv)
{
  struct simple_object_file *theobj;
  int fd;
  theobj = (struct simple_object_file*) malloc(sizeof(struct simple_object_file));
  theobj->theheader.magic = MH_MAGIC;
  theobj->theheader.cputype = CPU_TYPE_POWERPC;
  theobj->theheader.cpusubtype = CPU_SUBTYPE_POWERPC_ALL;
  theobj->theheader.filetype = MH_OBJECT;
  theobj->theheader.ncmds = 1;
  theobj->theheader.sizeofcmds = sizeof(struct segment_command) + sizeof(struct section);
  theobj->theheader.flags = MH_NOUNDEFS;
  theobj->thesegment.cmd = LC_SEGMENT;
  theobj->thesegment.cmdsize = sizeof(struct segment_command) + sizeof(struct section);
  memcpy(theobj->thesegment.segname, "", sizeof(char));
  theobj->thesegment.fileoff = sizeof(struct mach_header);
  theobj->thesegment.vmaddr = READ_ONLY_SPACE_START;
  theobj->thesegment.vmsize = READ_ONLY_SPACE_END - READ_ONLY_SPACE_START;
  theobj->thesegment.maxprot = VM_PROT_ALL;
  theobj->thesegment.initprot = VM_PROT_ALL;
  theobj->thesegment.nsects = 1;
  theobj->thesegment.flags = 0;
  memcpy(theobj->thesection.sectname, "core", 5);
  memcpy(theobj->thesection.segname, "SBCLRO", 7);
  theobj->thesection.addr = READ_ONLY_SPACE_START;
  theobj->thesection.size = READ_ONLY_SPACE_END - READ_ONLY_SPACE_START;
  theobj->thesection.offset = sizeof(struct mach_header) + sizeof(struct segment_command) + sizeof(struct section);
  theobj->thesection.align = 0;
  theobj->thesection.reloff = 0;
  theobj->thesection.nreloc = 0;
  theobj->thesection.flags = S_ZEROFILL | S_REGULAR | S_ATTR_SOME_INSTRUCTIONS;
  theobj->thesection.reserved1 = 0;
  theobj->thesection.reserved2 = 0;
  fd = open("ppc-darwin-rospace.o", O_WRONLY | O_CREAT, 0644);
  write(fd, theobj, sizeof(struct simple_object_file));
  close(fd);
  printf("-Wl,-segaddr,SBCLRO,0x%x\n",READ_ONLY_SPACE_START);
  return 0;
}
