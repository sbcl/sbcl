OUT = build

TARGETS = \
	test-bits \
	test-alloc \
	bench-alloc
TARGETS := $(addprefix $(OUT)/,$(TARGETS))

all: $(TARGETS)

CC = gcc
CFLAGS = \
	-std=c99 -Wall -g -I tlsf \
	-D TLSF_CONFIG_ASSERT
LDFLAGS =

OBJS = tlsf.o
OBJS := $(addprefix $(OUT)/,$(OBJS))
deps := $(OBJS:%.o=%.o.d)

$(OUT)/test-%: $(OBJS) tests/test-%.c
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
deps += build/test-bits.d

$(OUT)/bench-%: $(OBJS) tests/bench-%.c
	$(CC) $(CFLAGS) -o $@ -MMD -MF $@.d $^ $(LDFLAGS)
# FIXME: avoid hardcode
deps += build/bench-alloc.d

$(OUT)/%.o: tlsf/%.c
	@mkdir -p $(OUT)
	$(CC) $(CFLAGS) -c -o $@ -MMD -MF $@.d $<

CMDSEP = ; echo "Please wait..." ;
check: $(TARGETS)
	MALLOC_CHECK_=3 $(foreach prog,$(TARGETS),./$(prog) $(CMDSEP))

clean:
	$(RM) $(TARGETS) $(OBJS) $(deps)

.PHONY: all check clean

-include $(deps)
