# VRAM

I want to keep low VRAM free if one should want to use it for layer0.

I need a buffer space for saving screen memory when the top window is moved. I have decided that this buffer space will start at: **$1:8A80**

This will take 9600 bytes from the end of Sprite Image Data as I believe there is not many that would use sprites in a TUI except for the mouse pointer.

Every time a topmost window is drawn, the VRAM buffer needs to contain the data that will be overwritten by the window.
This also means that when a new top window is drawn, the buffer should be **cleared** and then filled with the data that is about to be overwritten by the new window.

