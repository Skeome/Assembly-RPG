;-------------------------------------------------------
; Assembly Program: RPG                                 |
; Target Platform: Linux x86_64                         |   
; Description: A classic RPG game                       |
; Assembler: NASM                                       |
; Author: Skeome                                        |
;-------------------------------------------------------

; This is the foundational skeleton.
; All complex logic is stubbed out with 'TODO' comments.
;----------------------------------------------------

bits 64
default rel

;----------------------------------------------------------------------
; SECTION .data (Initialized Data & Constants)
;----------------------------------------------------------------------
section .data
    ; --- Syscall Constants ---
    SYS_READ equ 0
    SYS_WRITE equ 1
    SYS_OPEN equ 2
    SYS_CLOSE equ 3
    SYS_MMAP equ 9
    SYS_IOCTL equ 16
    SYS_NANOSLEEP equ 35
    SYS_EXIT equ 60

    ; --- File Access Constants ---
    O_RDWR equ 0x2
    O_RDONLY equ 0x0
    O_NONBLOCK equ 0x800

    ; --- MMap Constants ---
    PROT_READ equ 0x1
    PROT_WRITE equ 0x2
    MAP_SHARED equ 0x1

    ; --- DRM/KMS Device Path ---
    dri_path db '/dev/dri/card0', 0

    ; --- Input Device Path ---
    ; NOTE: This is a placeholder! You will need to parse /proc/bus/input/devices
    ; or /dev/input/by-path/ to find the correct keyboard event file.
    input_event_path db '/dev/input/event0', 0

    ; --- DRM IOCTL Magic Numbers (PLACEHOLDERS) ---
    ; These must be defined from the kernel headers (e.g., <drm/drm.h>, <drm/drm_mode.h>)
    DRM_IOCTL_MODE_GETRESOURCES equ 0x...   ; TODO: Find value
    DRM_IOCTL_MODE_GETCONNECTOR equ 0x...   ; TODO: Find value
    DRM_IOCTL_MODE_CREATE_DUMB equ 0x...    ; TODO: Find value
    DRM_IOCTL_MODE_MAP_DUMB equ 0x...       ; TODO: Find value
    DRM_IOCTL_MODE_SET_CRTC equ 0x...       ; TODO: Find value
    DRM_IOCTL_MODE_ADDFB equ 0x...          ; TODO: Find value (for legacy framebuffer)
    DRM_IOCTL_MODE_RMFB equ 0x...           ; TODO: Find value
    DRM_IOCTL_MODE_GET_CRTC equ 0x...       ; TODO: Find value (to restore on exit)

    ; --- Game Strings ---
    msg_test_dialog db 'This is a test dialogue box. Press (Confirm).', 0
    msg_pause_menu db 'PAUSE', 0
    msg_menu_item1 db 'Items', 0
    msg_menu_item2 db 'Save', 0
    msg_menu_item3 db 'Quit', 0

    ; --- Tileset Data (Placeholders) ---
    ; We'll define 16x16 32-bit (ARGB) tiles.
    ; 16 * 16 = 256 pixels. 256 * 4 bytes/pixel = 1024 bytes per tile.
    ; Format: 0xAARRGGBB
    tile_grass:   times 256 dd 0xFF008000  ; Dark Green
    tile_wall:    times 256 dd 0xFF808080  ; Grey
    tile_water:   times 256 dd 0xFF0000FF  ; Blue

    ; Tileset pointer table for easy lookup
    ; Tile 0 = Grass, Tile 1 = Wall, Tile 2 = Water
    tileset_pointer_table:
        dq tile_grass
        dq tile_wall
        dq tile_water

    ; --- Tilemap Data (Placeholder Town) ---
    ; Defines the layout of the town using 1-byte tile IDs
    MAP_WIDTH  equ 10
    MAP_HEIGHT equ 8
    town_map:
        db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
        db 1, 0, 0, 0, 0, 0, 2, 2, 0, 1
        db 1, 0, 0, 0, 0, 0, 2, 2, 0, 1
        db 1, 0, 0, 1, 1, 0, 0, 0, 0, 1
        db 1, 0, 0, 1, 0, 0, 0, 0, 0, 1
        db 1, 0, 0, 1, 0, 0, 0, 0, 0, 1
        db 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
        db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

    ; --- Sprite Data (Placeholders) ---
    ; We'll use the 8x8 1-bit style from vpet.asm as a placeholder.
    ; This will be a *mask* - we'll use a solid color when drawing.
    sprite_player:
        db 0b00111100  ;  ####
        db 0b01111110  ; ######
        db 0b00100100  ;  #  #
        db 0b00111100  ;  ####
        db 0b01100110  ; ##  ##
        db 0b01100110  ; ##  ##
        db 0b00000000  ;
        db 0b00000000  ;
    sprite_npc:
        db 0b00111100  ;  ####
        db 0b01000010  ; #    #
        db 0b01000010  ; #    #
        db 0b00111100  ;  ####
        db 0b00011000  ;   ##
        db 0b00011000  ;   ##
        db 0b00111100  ;  ####
        db 0b00000000  ;

    ; --- Time Structure for nanosleep ---
    ; struct timespec { long tv_sec; long tv_nsec; };
    ; 1/60th of a second (approx 16.67ms)
    frame_delay_req:
        dq 0            ; tv_sec
        dq 16666667     ; tv_nsec
    frame_delay_rem:
        resb 16

;----------------------------------------------------------------------
; SECTION .bss (Uninitialized Data)
;----------------------------------------------------------------------
section .bss
    ; --- Graphics/DRM State ---
    gpu_fd resq 1
    framebuffer_pointer resq 1  ; This is our screen memory
    screen_width resd 1
    screen_height resd 1
    buffer_pitch resd 1         ; Bytes per horizontal line
    buffer_handle resd 1

    ; --- DRM Structs (Placeholders) ---
    ; We reserve space for the kernel to write into when we call ioctl
    ; NOTE: These sizes are guesstimates and must be verified from headers!
    drm_resources_struct resb 256
    drm_connector_struct resb 256
    drm_create_dumb_struct resb 32
    drm_map_dumb_struct resb 16
    drm_set_crtc_struct resb 256
    drm_old_crtc_struct resb 256    ; To restore the console on exit

    ; --- Input State ---
    input_fd resq 1
    input_event_struct resb 24      ; sizeof(struct input_event)
    key_state resb 256              ; 1-byte-per-key state table

    ; --- Game State ---
    ; 0 = Overworld
    ; 1 = Dialogue
    ; 2 = Pause Menu
    game_state resd 1
    
    ; Map tile coordinates
    player_x resd 1
    player_y resd 1

    ; Screen pixel coordinates
    camera_x resd 1
    camera_y resd 1

    ; Placeholder for a list of NPCs on the current map
    ; struct Npc { int x; int y; int sprite_ptr; int text_ptr; }
    npc_list resb 256

    ; Pause Menu State
    menu_cursor_pos resd 1
    submenu_state resd 1

;----------------------------------------------------------------------
; SECTION .text (Executable Code)
;----------------------------------------------------------------------
section .text
global _start

_start:
    ; 1. Initialize Graphics (DRM/KMS)
    call init_graphics
    cmp rax, 0
    jl error_exit           ; Exit if init_graphics failed

    ; 2. Initialize Input (Keyboard)
    call init_input
    cmp rax, 0
    jl error_exit           ; Exit if init_input failed

    ; 3. Initialize Game
    call init_game_state

; --- Main Game Loop ---
.game_loop:
    ; A. Read all pending input events
    call read_input

    ; B. Update game logic based on input and state
    call update_game_state

    ; C. Render the entire frame
    call render_frame
    
    ; D. Wait for 1/60th of a second
    mov rax, SYS_NANOSLEEP
    mov rdi, frame_delay_req
    mov rsi, frame_delay_rem
    syscall

    jmp .game_loop

;-------------------------------------------------
; Initialization Routines
;-------------------------------------------------
init_graphics:
    ; This is the most complex part. It involves a 5-phase setup.
    push rbp
    mov rbp, rsp
    
    ; TODO: Phase 1: Open /dev/dri/card0 (SYS_OPEN)
    ; Store fd in [gpu_fd]

    ; TODO: Phase 2: Find a valid screen and mode
    ;  a. ioctl(gpu_fd, DRM_IOCTL_MODE_GETRESOURCES, ...)
    ;  b. Parse resources to find a connector_id
    ;  c. ioctl(gpu_fd, DRM_IOCTL_MODE_GETCONNECTOR, ...)
    ;  d. Parse connector to find a valid mode (width/height)
    ;  e.Parse connector to find a valid encoder_id
    ;  f. Parse resources to find a crtc_id
    
    ; TODO: Phase 3: Create a "Dumb Buffer"
    ;  a. Fill drm_create_dumb_struct (width, height, 32bpp)
    ;  b. ioctl(gpu_fd, DRM_IOCTL_MODE_CREATE_DUMB, ...)
    ;  c. Store buffer_handle, buffer_pitch, and size from struct
    ;  d. Store width/height in [screen_width]/[screen_height]

    ; TODO: Phase 4: Set the Mode (Go Fullscreen)
    ;  a. ioctl(gpu_fd, DRM_IOCTL_MODE_GET_CRTC, &drm_old_crtc_struct) ; Save old mode
    ;  b. Fill drm_set_crtc_struct (crtc_id, buffer_handle, mode)
    ;  c. ioctl(gpu_fd, DRM_IOCTL_MODE_SET_CRTC, ...)

    ; TODO: Phase 5: Map the buffer to our memory
    ;  a. Fill drm_map_dumb_struct (buffer_handle)
    ;  b. ioctl(gpu_fd, DRM_IOCTL_MODE_MAP_DUMB, ...)
    ;  c. Get 'offset' from struct
    ;  d. mov rax, SYS_MMAP
    ;     rdi=0, rsi=size, rdx=PROT_READ|PROT_WRITE, r10=MAP_SHARED, r8=gpu_fd, r9=offset
    ;     syscall
    ;  e. Store rax (the pointer) in [framebuffer_pointer]

    ; If any step fails, we must 'ret' with rax = -1
    ; On success:
    mov rax, 0  ; Success
    mov rsp, rbp
    pop rbp
    ret

init_input:
    ; Open the keyboard event device
    push rbp
    mov rbp, rsp

    mov rax, SYS_OPEN
    mov rdi, input_event_path
    mov rsi, O_RDONLY | O_NONBLOCK ; Non-blocking
    syscall

    ; rax holds the file descriptor (or -1 on error)
    mov [input_fd], rax

    mov rsp, rbp
    pop rbp
    ret

init_game_state:
    mov dword [game_state], 0   ; 0 = Overworld
    mov dword [player_x], 3
    mov dword [player_y], 3
    mov dword [camera_x], 0
    mov dword [camera_y], 0
    ret

;-------------------------------------------------
; Input Handling
;-------------------------------------------------
read_input:
    ; Read all pending events from the non-blocking file descriptor
.read_loop:
    mov rax, SYS_READ
    mov rdi, [input_fd]
    mov rsi, input_event_struct
    mov rdx, 24 ; sizeof(struct input_event)
    syscall
    
    cmp rax, 24 ; Did we read a full event?
    jl .done

    ; TODO: Parse the input_event_struct
    ;   WORD type = [input_event_struct + 16]
    ;   WORD code = [input_event_struct + 18]
    ;   DWORD value = [input_event_struct + 20]
    
    ;   if (type == EV_KEY)
    ;       [key_state + code] = value (1=press, 0=release, 2=hold)
    
    jmp .read_loop
.done:
    ret

;-------------------------------------------------
; Game Logic / State Update
;-------------------------------------------------
update_game_state:
    ; Check game_state and dispatch
    mov eax, [game_state]
    cmp eax, 0
    je .update_overworld
    cmp eax, 1
    je .update_dialogue
    cmp eax, 2
    je .update_menu
    ret
    
.update_overworld:
    ; TODO: Check [key_state] for KEY_UP, KEY_DOWN, etc.
    ;   - On press, update [player_x] / [player_y]
    ;   - Check for collisions against town_map
    ;   - Check for NPC interactions
    ;   - Check for (Confirm) -> change state to 1 (Dialogue)
    ;   - Check for (Menu) -> change state to 2 (Pause)
    ret
    
.update_dialogue:
    ; TODO: Check [key_state] for KEY_A (Confirm)
    ;   - On press, advance dialogue
    ;   - If dialogue ends, set [game_state] = 0 (Overworld)
    ret
    
.update_menu:
    ; TODO: Check [key_state] for KEY_UP, KEY_DOWN
    ;   - Update [menu_cursor_pos]
    ;   - Check for (Confirm) -> e.g., enter submenu
    ;   - Check for (Return/Menu) -> set [game_state] = 0 (Overworld)
    ret

;-------------------------------------------------
; Main Render Dispatch
;-------------------------------------------------
render_frame:
    ; This is the master render function
    
    ; 1. Clear the screen (or just overdraw)
    ; call render_clear_screen
    
    ; 2. Render the world (tilemap)
    call render_tilemap
    
    ; 3. Render NPCs
    call render_npcs
    
    ; 4. Render Player
    call render_player
    
    ; 5. Render UI (if any)
    mov eax, [game_state]
    cmp eax, 1
    je .render_ui_dialogue
    cmp eax, 2
    je .render_ui_menu
    ret

.render_ui_dialogue:
    call render_dialogue_box
    ret
.render_ui_menu:
    call render_pause_menu
    ret

;-------------------------------------------------
; Low-Level Render Routines (Stubs)
;-------------------------------------------------
render_clear_screen:
    ; TODO: Fill the entire [framebuffer_pointer] with 0x00000000 (black)
    ; Use 'rep stosd' for speed.
    ret

render_tilemap:
    ; TODO:
    ; 1. Get [camera_x], [camera_y]
    ; 2. Loop from y=0 to SCREEN_HEIGHT/16
    ; 3.   Loop from x=0 to SCREEN_WIDTH/16
    ; 4.     Calculate map_x = (x * 16 + camera_x) / 16
    ; 5.     Calculate map_y = (y * 16 + camera_y) / 16
    ; 6.     Get tile_id = [town_map + map_y * MAP_WIDTH + map_x]
    ; 7.     Get tile_data_ptr = [tileset_pointer_table + tile_id * 8]
    ; 8.     call draw_tile(tile_data_ptr, x*16, y*16)
    ret
    
render_npcs:
    ; TODO:
    ; 1. Loop through [npc_list]
    ; 2. Get npc.x, npc.y, npc.sprite_ptr
    ; 3. Calculate screen_x = npc.x - camera_x
    ; 4. Calculate screen_y = npc.y - camera_y
    ; 5. call draw_sprite(npc.sprite_ptr, screen_x, screen_y, 0xFF808080) ; Grey color
    ret

render_player:
    ; TODO:
    ; 1. Get [player_x], [player_y]
    ; 2. Calculate screen_x = player_x*16 - camera_x
    ; 3. Calculate screen_y = player_y*16 - camera_y
    ; 4. call draw_sprite(sprite_player, screen_x, screen_y, 0xFFFFFFFF) ; White color
    ret
    
render_dialogue_box:
    ; TODO:
    ; 1. call draw_rect(x=10, y=SCREEN_HEIGHT-100, w=SCREEN_WIDTH-20, h=90, color=0xCC000080) ; Translucent blue
    ; 2. call draw_text(msg_test_dialog, x=20, y=SCREEN_HEIGHT-90, color=0xFFFFFFFF)
    ret
    
render_pause_menu:
    ; TODO:
    ; 1. call draw_rect(x=10, y=10, w=200, h=300, color=0xCC000000) ; Translucent black
    ; 2. call draw_text(msg_pause_menu, x=20, y=20, color=0xFFFFFFFF)
    ; 3. call draw_text(msg_menu_item1, x=40, y=50, ...)
    ; 4. call draw_text(msg_menu_item2, x=40, y=70, ...)
    ; 5. call draw_text(msg_menu_item3, x=40, y=90, ...)
    ; 6. Get [menu_cursor_pos]
    ; 7. call draw_sprite(sprite_cursor, x=20, y=50 + cursor_pos*20, color=0xFFFFFF00)
    ret
    
;-------------------------------------------------
; Drawing Primitives (Stubs)
;-------------------------------------------------
; Args: RDI=tile_data_ptr, RSI=screen_x, RDX=screen_y
draw_tile:
    ; TODO:
    ; 1. Calculate base_addr = [framebuffer_pointer] + (RDX * [buffer_pitch]) + (RSI * 4)
    ; 2. Loop 16 times (for y=0 to 15)
    ; 3.   memcpy(base_addr, RDI, 16 * 4) ; Copy one row
    ; 4.   RDI += (16 * 4) ; Advance tile data ptr
    ; 5.   base_addr += [buffer_pitch] ; Advance screen ptr
    ret

; Args: RDI=sprite_mask_ptr, RSI=screen_x, RDX=screen_y, RCX=color (32-bit ARGB)
draw_sprite:
    ; TODO:
    ; 1. Calculate base_addr = [framebuffer_pointer] + (RDX * [buffer_pitch]) + (RSI * 4)
    ; 2. Loop 8 times (for y=0 to 7)
    ; 3.   mov al, [RDI+y] ; Get 8-bit mask row
    ; 4.   Loop 8 times (for x=0 to 7)
    ; 5.     shl al, 1
    ; 6.     jnc .skip_pixel
    ; 7.     mov [base_addr + x*4], ecx ; Write color
    ; 8.   .skip_pixel:
    ; 9.   base_addr += [buffer_pitch]
    ret
    
; Args: RDI=x, RSI=y, RDX=w, RCX=h, R8=color
draw_rect:
    ; TODO:
    ; 1. Calculate base_addr = [framebuffer_pointer] + (RSI * [buffer_pitch]) + (RDI * 4)
    ; 2. Loop RCX times (height)
    ; 3.   Loop RDX times (width)
    ; 4.     mov [base_addr + x*4], r8d
    ; 5.   base_addr += [buffer_pitch]
    ret
    
; Args: RDI=string_ptr, RSI=x, RDX=y, R8=color
draw_text:
    ; TODO:
    ; 1. Loop [RDI] char by char
    ; 2.   call draw_char(char, RSI, RDX, R8)
    ; 3.   RSI += 8 ; Advance X
    ret

;-------------------------------------------------
; Exit Routines
;-------------------------------------------------
clean_exit:
    ; TODO: Restore the original graphics mode!
    ;  a. ioctl(gpu_fd, DRM_IOCTL_MODE_SET_CRTC, &drm_old_crtc_struct)

    ; TODO: Unmap framebuffer
    ;  a. mov rax, SYS_MUNMAP, rdi=framebuffer_pointer, rsi=size

    ; TODO: Close file descriptors
    ;  a. call SYS_CLOSE with [gpu_fd]
    ;  b. call SYS_CLOSE with [input_fd]

    mov rax, SYS_EXIT
    mov rdi, 0 ; EXIT_SUCCESS
    syscall

error_exit:
    ; TODO: We should still try to restore the console if possible
    
    mov rax, SYS_EXIT
    mov rdi, 1 ; EXIT_FAILURE
    syscall