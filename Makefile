
PROJECT	:= x16vision
AS		:= ca65
LD		:= ld65
AR		:= ar65
MKDIR	:= mkdir -p
RMDIR	:= rmdir -p
ASFLAGS	:= --cpu 65C02 -g
SRCS	:= $(wildcard *.s)
OBJS    := $(patsubst %.s,%.o,$(SRCS))
BIN     := x16vision.bin
MAPFILE := ./$(PROJECT).map
SYMFILE := ./$(PROJECT).sym


default: all

all: $(BIN)

$(BIN): $(OBJS)
	$(LD) $(LDFLAGS) -m $(MAPFILE) -Ln $(SYMFILE) -C x16vision.cfg $(OBJS) -o $@

%.o: %.s
	$(AS) $(ASFLAGS) $(DEFINES) $< -o $@

.PHONY: clean run
clean:
	$(RM) $(OBJS) $(BIN)
