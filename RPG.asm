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
    SYS_MUNMAP equ 11
    SYS_IOCTL equ 16
    SYS_RT_SIGACTION equ 13
    SYS_NANOSLEEP equ 35
    SYS_EXIT equ 60

    ; --- Signal Constants ---
    SIGINT equ 2                ; Signal 2 = Interrupt (Ctrl+C)
    SA_RESTORER equ 0x04000000  ; Required flag

    ; --- File Access Constants ---
    O_RDWR equ 0x2
    O_RDONLY equ 0x0
    O_NONBLOCK equ 0x800

    ; --- MMap Constants ---
    PROT_READ equ 0x1
    PROT_WRITE equ 0x2
    MAP_SHARED equ 0x1

    ; --- DRM/KMS Device Path ---
    dri_path db '/dev/dri/card1', 0 ; <-- FIX: Using card1 as per user's system

    ; --- Input Device Path ---
    ; NOTE: This is a placeholder! You will need to parse /proc/bus/input/devices
    ; or /dev/input/by-path/ to find the correct keyboard event file.
    input_event_path db '/dev/input/by-path/platform-i8042-serio-0-event-kbd', 0

    ; --- DRM IOCTL Magic Numbers (Correct for x86_64) ---
    ; _IOWR(DRM_COMMAND_BASE, 0xA0, struct drm_mode_card_res)
    DRM_IOCTL_MODE_GETRESOURCES equ 0xC04064A0
    ; _IOWR(DRM_COMMAND_BASE, 0xA7, struct drm_mode_get_connector)
    ; --- FIX: Sizeof(struct) is 96 (0x60), not 64 (0x40) ---
    DRM_IOCTL_MODE_GETCONNECTOR equ 0xC06064A7
    ; _IOWR(DRM_COMMAND_BASE, 0xB2, struct drm_mode_create_dumb)
    DRM_IOCTL_MODE_CREATE_DUMB equ 0xC02064B2
    ; _IOWR(DRM_COMMAND_BASE, 0xB3, struct drm_mode_map_dumb)
    DRM_IOCTL_MODE_MAP_DUMB equ 0xC01064B3
    ; _IOWR(DRM_COMMAND_BASE, 0xA2, struct drm_mode_crtc)
    DRM_IOCTL_MODE_SET_CRTC equ 0xC05864A2
    ; _IOWR(DRM_COMMAND_BASE, 0xA1, struct drm_mode_crtc)
    DRM_IOCTL_MODE_GET_CRTC equ 0xC05864A1
    ; _IOWR(DRM_COMMAND_BASE, 0xA6, struct drm_mode_get_encoder)
    DRM_IOCTL_MODE_GETENCODER equ 0xC01C64A6
    ; _IOWR(DRM_COMMAND_BASE, 0xA8, struct drm_mode_fb_cmd)
    DRM_IOCTL_MODE_ADDFB equ 0xC01C64A8
    ; _IOWR(DRM_COMMAND_BASE, 0xA9, struct drm_mode_rm_fb)
    DRM_IOCTL_MODE_RMFB equ 0x800464A9

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

    ; --- SIGACTION Structure ---
    ; struct sigaction { void (*sa_handler)(int); unsigned long sa_flags; ... }
    sa_handler_struct:
        dq clean_exit       ; sa_handler (pointer to our function)
        dq SA_RESTORER      ; sa_flags
        dq 0                ; sa_restorer (unused in simple cases)

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
    buffer_size resq 1          ; 64-bit total size of the buffer
    
    ; Placeholder for nanosleep
    frame_delay_rem:
        resb 16
    
    ; --- DRM Structs (Placeholders) ---
    ; We reserve space for the kernel to write into when we call ioctl
    ; NOTE: These sizes MUST be correct for the kernel ABI
    
    ; We need pointers for the kernel to write the list addresses into
    res_connector_ptr resq 1
    res_crtc_ptr resq 1
    
    ; struct drm_mode_card_res (size 64 bytes)
    drm_resources_struct:
        resq 1  ; fb_id_ptr (offset 0)
        resq 1  ; crtc_id_ptr (offset 8)
        resq 1  ; connector_id_ptr (offset 16)
        resq 1  ; encoder_id_ptr (offset 24)
        resd 1  ; count_fbs (offset 32)
        resd 1  ; count_crtcs (offset 36)
        resd 1  ; count_connectors (offset 40)
        resd 1  ; count_encoders (offset 44)
        resd 1  ; min_width (offset 48)
        resd 1  ; max_width (offset 52)
        resd 1  ; min_height (offset 56)
        resd 1  ; max_height (offset 60)

    ; Pointers to the lists themselves (we need 16 entries max for now)
    ; These are arrays of 32-bit IDs
    connector_id_list resd 16
    crtc_id_list resd 16
    encoder_id_list resd 16

    ; We use a generous 256 bytes. The kernel fills this.
    drm_connector_struct resb 256
    drm_encoder_struct resb 28     ; sizeof(struct drm_mode_get_encoder)
    drm_create_dumb_struct resb 32  ; sizeof(struct drm_mode_create_dumb)
    drm_map_dumb_struct resb 16     ; sizeof(struct drm_mode_map_dumb)
    
    ; struct drm_mode_crtc (size 88 bytes)
    drm_set_crtc_struct resb 88
    drm_old_crtc_struct resb 88     ; To restore the console on exit

    ; --- DRM Results (to store what we find) ---
    selected_connector_id resd 1
    selected_crtc_id resd 1
    selected_mode resb 32           ; sizeof(struct drm_mode_modeinfo)

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
    ; 0. Setup Ctrl+C (SIGINT) handler
    call setup_sigint_handler

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
    push rbx                ; Save callee-saved registers
    push r12
    push r13
    push r14
    push r15
    
    ; --- Phase 1: Open /dev/dri/card1 (SYS_OPEN) ---
    mov rax, SYS_OPEN
    mov rdi, dri_path
    mov rsi, O_RDWR
    syscall
    
    cmp rax, 0
    jl .init_fail           ; If rax is negative, open failed
    mov [gpu_fd], rax
    mov rbx, rax            ; Keep gpu_fd in rbx for convenience

    ; --- Phase 2a: Get Hardware Resources (CRTCs, Connectors) ---
    
    ; We need to set the pointers in the struct *before* the call
    mov rax, connector_id_list
    mov [drm_resources_struct + 16], rax ; connector_id_ptr
    
    mov rax, crtc_id_list
    mov [drm_resources_struct + 8], rax  ; crtc_id_ptr
    
    mov rax, encoder_id_list
    mov [drm_resources_struct + 24], rax ; encoder_id_ptr
    
    ; --- FIX: Set the counts to the size of our lists (16) ---
    mov dword [drm_resources_struct + 32], 0  ; count_fbs (we don't need)
    mov dword [drm_resources_struct + 36], 16 ; count_crtcs
    mov dword [drm_resources_struct + 40], 16 ; count_connectors
    mov dword [drm_resources_struct + 44], 16 ; count_encoders
    
    mov rax, SYS_IOCTL
    mov rdi, rbx            ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_GETRESOURCES
    mov rdx, drm_resources_struct
    syscall
    
    cmp rax, 0
    jl .init_fail           ; If ioctl failed, exit

    ; --- Phase 2b/2c/2d: Find a "Connected" Monitor (Connector) ---
    mov r12d, [drm_resources_struct + 40] ; r12d = number of connectors found
    xor r13, r13                          ; r13 = loop counter (i)
    
.connector_loop:
    cmp r13, r12                ; Have we checked all connectors?
    jae .no_connector_found     ; If yes, fail (no monitor plugged in)

    ; 1. Get connector_id from the list
    ; --- FIX: Kernel array is 32-bit (dword), so multiply by 4 ---
    mov r15d, [connector_id_list + r13*4] ; r15d = connector_id
    inc r13                             ; (i++)
    
    ; --- FIX: Zero out the struct before use ---
    lea rdi, [drm_connector_struct]
    mov rcx, 256 / 8 ; 32 qwords
    xor rax, rax
    rep stosq
    ; --- END FIX ---
    
    ; 2. Call GETCONNECTOR to get its info
    ; Set the connector_id field (offset 8)
    mov [drm_connector_struct + 8], r15d ; <-- FIX: Use r15d
    
    mov rax, SYS_IOCTL
    mov rdi, rbx                ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_GETCONNECTOR
    mov rdx, drm_connector_struct
    syscall
    
    cmp rax, 0
    jl .connector_loop          ; If ioctl failed, try next connector
    
    ; 3. Check connection status (offset 16)
    mov eax, [drm_connector_struct + 16]
    cmp eax, 1                  ; 1 = DRM_MODE_CONNECTED
    jne .connector_loop         ; If not 1, try next connector

    ; 4. Check that it has at least one mode (offset 36)
    mov eax, [drm_connector_struct + 36]
    cmp eax, 0
    jle .connector_loop         ; If 0 modes, try next connector

    ; --- Success: We found a connected monitor! ---
    ; 5. Save the connector_id
    mov eax, [drm_connector_struct + 8]
    mov [selected_connector_id], eax
    
    ; 6. Save the first available mode (offset 40)
    ; struct drm_mode_modeinfo is 32 bytes
    lea rsi, [drm_connector_struct + 40] ; rsi = pointer to first mode
    lea rdi, [selected_mode]             ; rdi = destination
    mov rcx, 32                          ; 32 bytes
    rep movsb
    
.connector_found: ; <-- Breakpoint 403
    ; --- Phase 2e/2f: Find a compatible Encoder and CRTC ---
    ; We have a list of encoders for our connector in [encoder_id_list]
    ; We have a list of all CRTCs in [crtc_id_list]
    ; We must find an encoder that has a bitmask entry for a valid CRTC.
.no_connector_found:
    ; This jump happens if r13 >= r12
    cmp r13, r12
    jge .init_fail ; If we checked all and found none, fail

    mov r12d, [drm_connector_struct + 32] ; r12d = count_encoders
    mov r13, encoder_id_list             ; r13 = encoder_id_list pointer
    xor r14, r14                         ; r14 = outer loop counter (i)
    
.encoder_loop:
    cmp r14, r12                ; Have we checked all encoders?
    jae .no_crtc_found          ; If yes, fail (no compatible encoder/crtc)
    
    ; 1. Get an encoder_id from our connector's list
    ; --- FIX: Kernel array is 32-bit (dword), so multiply by 4 ---
    mov edi, [r13 + r14*4]
    inc r14                     ; (i++)
    
    ; 2. Call GETENCODER to get its info
    ; --- FIX: Zero out the struct ---
    lea rsi, [drm_encoder_struct]
    mov rcx, 28 / 4 ; 7 dwords
    xor rax, rax
    rep stosd
    ; --- END FIX ---
    mov dword [drm_encoder_struct + 4], edi ; Set encoder_id field (offset 4)
    
    mov rax, SYS_IOCTL
    mov rdi, rbx                            ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_GETENCODER
    mov rdx, drm_encoder_struct
    syscall
    
    cmp rax, 0
    jl .encoder_loop        ; If ioctl failed, try next encoder
    
    ; 3. Get the 'possible_crtcs' bitmask from the encoder struct
    mov r10d, [drm_encoder_struct + 12] ; r10d = possible_crtcs (offset 12)
    
    ; 4. Check this bitmask against our list of CRTCs
    mov r15d, [drm_resources_struct + 36] ; r15d = res_count_crtcs
    xor r9, r9                           ; r9 = inner loop counter (j)
    
.crtc_loop:
    cmp r9, r15                 ; Have we checked all CRTCs?
    jae .encoder_loop           ; If yes, this encoder failed, try next one
    
    ; Check if bit 'j' is set in 'possible_crtcs'
    mov rax, r10                ; Use rax for bit test
    mov cl, r9b                 ; Move 8-bit loop counter 'j' into CL
    shr rax, cl                 ; Shift rax right by the amount in CL
    and rax, 1                  ; Isolate the bit
    
    cmp rax, 1
    jne .next_crtc              ; Bit not set, this CRTC is not compatible
    
    ; --- Success: We found a match! ---
    ; The 'j'th CRTC in [crtc_id_list] is compatible.
    ; --- FIX: Kernel array is 32-bit (dword), so multiply by 4 ---
    mov eax, [crtc_id_list + r9*4]
    mov [selected_crtc_id], eax ; Save the 32-bit ID
    
    jmp .crtc_found        ; We are done with Phase 2
    
.next_crtc:
    inc r9                      ; (j++)
    jmp .crtc_loop

.no_crtc_found:
    ; We looped all encoders and found no compatible CRTC
    jmp .init_fail

.crtc_found:
    ; --- Phase 3: Create a "Dumb Buffer" ---
    
    ; 3a. Fill drm_create_dumb_struct
    ; We get the width (hdisplay) from the mode info (offset 16)
    movzx eax, word [selected_mode + 16]
    mov [drm_create_dumb_struct + 4], eax   ; Set width
    mov [screen_width], eax                 ; Save for game logic
    
    ; We get the height (vdisplay) from the mode info (offset 18)
    movzx eax, word [selected_mode + 18]
    mov [drm_create_dumb_struct + 0], eax   ; Set height
    mov [screen_height], eax                ; Save for game logic
    
    ; Set 32 bits per pixel (4 bytes)
    mov dword [drm_create_dumb_struct + 8], 32
    ; Set flags to 0
    mov dword [drm_create_dumb_struct + 12], 0
    
    ; 3b. Call ioctl to create the buffer
    mov rax, SYS_IOCTL
    mov rdi, rbx                            ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_CREATE_DUMB
    mov rdx, drm_create_dumb_struct
    syscall
    
    cmp rax, 0
    jl .init_fail           ; Fail if ioctl error
    
    ; 3c. Store the results from the struct
    ; The kernel filled in handle, pitch, and size
    mov eax, [drm_create_dumb_struct + 16]
    mov [buffer_handle], eax
    
    mov eax, [drm_create_dumb_struct + 20]
    mov [buffer_pitch], eax
    
    mov rax, [drm_create_dumb_struct + 24]
    mov [buffer_size], rax

    ; --- Phase 4: Set the Mode (Go Fullscreen) ---
    
    ; 4a. Save the current CRTC settings so we can restore on exit
    ; We need to set the crtc_id in the struct (offset 8)
    mov eax, [selected_crtc_id]
    mov [drm_old_crtc_struct + 8], eax
    
    mov rax, SYS_IOCTL
    mov rdi, rbx                        ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_GET_CRTC
    mov rdx, drm_old_crtc_struct
    syscall
    ; We don't care if this fails, but we must try.
    
    ; 4b. Fill the SET_CRTC struct to apply our new mode
    ; Zero out the struct first
    mov rdi, drm_set_crtc_struct
    mov rcx, 88 / 8 ; 11 qwords
    xor rax, rax
    rep stosq
    
    ; Set crtc_id (offset 8)
    mov eax, [selected_crtc_id]
    mov [drm_set_crtc_struct + 8], eax
    
    ; Set buffer_handle (offset 12)
    mov eax, [buffer_handle]
    mov [drm_set_crtc_struct + 12], eax
    
    ; Set connector_id (offset 24)
    mov eax, [selected_connector_id]
    mov [drm_set_crtc_struct + 24], eax
    
    ; Set connector_count (offset 20)
    mov dword [drm_set_crtc_struct + 20], 1
    
    ; Copy the 32-byte mode info struct (offset 28)
    lea rsi, [selected_mode]
    lea rdi, [drm_set_crtc_struct + 28]
    mov rcx, 32
    rep movsb
    
    ; 4c. Call ioctl to SET the CRTC
    mov rax, SYS_IOCTL
    mov rdi, rbx                        ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_SET_CRTC
    mov rdx, drm_set_crtc_struct
    syscall
    
    cmp rax, 0
    jl .init_fail           ; Fail if ioctl error
    
    ; --- Phase 5: Map the buffer to our memory ---
    
    ; 5a. Fill drm_map_dumb_struct
    ; We need to set the handle (offset 0)
    mov eax, [buffer_handle]
    mov [drm_map_dumb_struct + 0], eax
    
    ; 5b. Call ioctl to get the memory offset for mmap
    mov rax, SYS_IOCTL
    mov rdi, rbx                        ; rdi = gpu_fd
    mov rsi, DRM_IOCTL_MODE_MAP_DUMB
    mov rdx, drm_map_dumb_struct
    syscall
    
    cmp rax, 0
    jl .init_fail           ; Fail if ioctl error
    
    ; 5c. Get the 'offset' from the struct (offset 8)
    mov r9, [drm_map_dumb_struct + 8]   ; r9 is 6th arg for mmap
    
    ; 5d. Call mmap
    mov rax, SYS_MMAP
    mov rdi, 0                          ; Let kernel choose address
    mov rsi, [buffer_size]              ; Size of buffer
    mov rdx, PROT_READ | PROT_WRITE     ; 0x3
    mov r10, MAP_SHARED                 ; 0x1
    mov r8, rbx                         ; gpu_fd
    ; r9 already holds the offset
    syscall
    
    ; 5e. Store the pointer
    ; mmap returns MAP_FAILED (-1) on error.
    cmp rax, -1
    je .init_fail
    
    mov [framebuffer_pointer], rax
    
    ; If any step fails, we must 'ret' with rax = -1
    ; On success:
    mov rax, 0  ; Success
    
.init_cleanup:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov rsp, rbp
    pop rbp
    ret

.init_fail:
    mov rax, -1 ; Return -1 on failure
    jmp .init_cleanup

init_input:
    ; Open the keyboard event device
    push rbp
    mov rbp, rsp

    mov rax, SYS_OPEN
    mov rdi, input_event_path
    mov rsi, O_RDONLY | O_NONBLOCK ; Non-blocking
    syscall

    ; rax holds the file descriptor (or -1 on error)
    cmp rax, 0
    jl .input_fail
    mov [input_fd], rax
    
    pop rbp
    ret
    
.input_fail:
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
    call render_clear_screen
    
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
    ; Fills the entire [framebuffer_pointer] with 0x00000000 (black)
    ; Use 'rep stosd' for speed.
    push rdi
    push rcx
    push rax

    mov rdi, [framebuffer_pointer]  ; RDI = destination
    
    ; Calculate total number of 32-bit pixels (DWORDS)
    ; size (in bytes) / 4
    mov rax, [buffer_size]
    shr rax, 2                      ; Divide by 4
    mov rcx, rax                    ; RCX = count
    
    xor eax, eax                    ; EAX = 0x00000000 (the color black)
    
    rep stosd                       ; Fill RCX dwords at [RDI] with EAX

    pop rax
    pop rcx
    pop rdi
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
    ; 7. call draw_sprite(sprite_cursor, x=20, y=50 + cursor_pos*20, color=0xFFFFFF00) ; TODO: Implement this
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
    ; 5.     mov cl, 7
    ; 6.     sub cl, bl ; bl is x (0-7)
    ; 7.     shr al, cl ; Shift bit 'x' into lowest position
    ; 8.     and al, 1  ; Isolate bit
    ; 9.     cmp al, 0
    ; 10.    je .skip_pixel
    ; 11.    mov [base_addr + x*4], ecx ; Write color
    ; 12.  .skip_pixel:
    ; 13.  base_addr += [buffer_pitch]
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
; Signal Handler (for Ctrl+C)
;-------------------------------------------------
setup_sigint_handler:
    push rbp
    mov rbp, rsp
    
    mov rax, SYS_RT_SIGACTION
    mov rdi, SIGINT             ; Signal number (2)
    mov rsi, sa_handler_struct  ; Pointer to new sigaction struct
    mov rdx, 0                  ; No need to retrieve old action
    mov r10, 8                  ; Size of sigset_t (8 bytes)
    syscall                     ; Execute syscall
    
    mov rsp, rbp
    pop rbp
    ret

;-------------------------------------------------
; Exit Routines
;-------------------------------------------------
clean_exit:
    ; This function is now called by SIGINT or by normal exit
    
    ; We must be careful here, as some of these resources
    ; might not have been acquired if init failed.
    
    ; 1. Restore the original graphics mode
    ; Check if gpu_fd is valid (greater than 0)
    mov rdi, [gpu_fd]
    cmp rdi, 0
    jle .skip_modeset_restore

    mov rax, SYS_IOCTL
    ; rdi is already gpu_fd
    mov rsi, DRM_IOCTL_MODE_SET_CRTC
    mov rdx, drm_old_crtc_struct
    syscall
    ; We don't care about the return value, we're exiting anyway.
.skip_modeset_restore:

    ; 2. Unmap framebuffer
    mov rdi, [framebuffer_pointer]
    cmp rdi, 0
    jle .skip_munmap
    
    mov rax, SYS_MUNMAP
    ; rdi is already framebuffer_pointer
    mov rsi, [buffer_size]
    syscall
.skip_munmap:

    ; 3. Close file descriptors
    mov rdi, [gpu_fd]
    cmp rdi, 0
    jle .skip_gpu_close
    
    mov rax, SYS_CLOSE
    ; rdi is already gpu_fd
    syscall
.skip_gpu_close:

    mov rdi, [input_fd]
    cmp rdi, 0
    jle .skip_input_close

    mov rax, SYS_CLOSE
    ; rdi is already input_fd
    syscall
.skip_input_close:
    
    ; 4. Exit
    mov rax, SYS_EXIT
    mov rdi, 0 ; EXIT_SUCCESS
    syscall

error_exit:
    ; In a real app, we'd print an error to stderr first
    ; For now, just exit with an error code
    
    ; We should still ATTEMPT to restore the console,
    ; but we'll skip that for this skeleton.
    
    mov rax, SYS_EXIT
    mov rdi, 1 ; EXIT_FAILURE
    syscall