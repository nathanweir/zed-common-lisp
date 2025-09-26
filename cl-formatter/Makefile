OUTPUT_BIN=cl-formatter
ROS_SCRIPT=ros/cl-formatter.ros

.PHONY: all build clean

all: build

build:
	ROS_OPTS="--no-rc --disable-debugger" ros build $(ROS_SCRIPT) 2>&1 | awk '{if ($$0 ~ /\[ERROR\]/) {print "\033[1;31m" $$0 "\033[0m"} else {print $$0}}'

clean:
	rm -f $(OUTPUT_BIN)
	rm -rf *.fasl *.x86f *.dx86f *.lx86f *.fx86f *.sse2f *.o *.so *.dylib *.dll
	rm -rf qlot/ local-projects/
	rm -rf quicklisp/
