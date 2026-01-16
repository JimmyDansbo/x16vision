# VRAM

I want to keep low VRAM free if one should want to use it for layer0.

I need a buffer space for saving screen memory when the top window is moved. I have decided that this buffer space will start at: **$1:8A80**

This will take 9600 bytes from the end of Sprite Image Data as I believe there is not many that would use sprites in a TUI except for the mouse pointer.

Every time a topmost window is drawn, the VRAM buffer needs to contain the data that will be overwritten by the window.
This also means that when a new top window is drawn, the buffer should be **cleared** and then filled with the data that is about to be overwritten by the new window.

Any action, other than movinge a window, i.e. moving a window from background to foreground will trigger a redraw of all items that were in front of that window

# Banked memory

Library is designed to be loaded into banked memory.  
Initially, the library will use the remaining free space in the bank it is loaded into to dynamically allocate memory for item metadata like z-order, coordinates, height, width etc.

If the library bank gets full, the library must be able to handle using another RAM bank to dynamically allocate memory for metadata. It is assumed that a newly given RAM bank will be fully available to the library.

256 bytes of lowram must be provided to the library for helper functions like Interrupt Service Routine (ISR), banked fetch and store routines etc.

# Memory structures

At the very beginning of an allocated RAM bank ($A000), must be the number of available bytes in that bank, occupying 2 bytes.
The next 2 bytes must contain the address if the first available byte in that RAM bank. For an empty RAM bank that address would be $A004.
This is done to enable the x16vision library to use the same bank as it is stored in for allocating memory for objects.
