run:
	as -arch arm64 -o out.o aarch64.S
	ld -arch arm64 -o out out.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _start
