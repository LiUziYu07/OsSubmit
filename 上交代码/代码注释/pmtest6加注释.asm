; ==========================================
; pmtest6.asm
; 编译方法：nasm pmtest6.asm -o pmtest6.com
; ==========================================

%include	"pm.inc"	; 常量, 宏, 以及一些说明(64位的描述符图示和说明、16位的选择子图示和说明、分页机制使用的常量说明、描述符Descriptor宏、门Gate宏)

PageDirBase		equ	200000h	; 页目录开始地址: 2M
PageTblBase		equ	201000h	; 页表开始地址: 2M+4K ，因为页目录表正好占据4K

org	0100h  ;告诉编译器，这段程序要被加载到内存偏移地址0x0100h，段的前0100h（256字节）是留给程序段前缀PSP的。这是操作系统DOS的概念
		   ;对于一些引导程序，这些程序将被BIOS装载到内存中的指定地方，通常为0000:7c00，DOS是将COM文件的代码装载到CS:0100处
	jmp	LABEL_BEGIN   ;跳转到LABEL_BEGIN处开始执行，此时还处于实模式中

[SECTION .gdt]  ;将以下代码或数据分配到gdt段，SECTION默认4字节对齐的：
; GDT           ;Descriptor是在pm.inc（267~278）中定义的描述符宏  Descriptor Base, Limit, Attr   共8字节64位 Base 32+Limit 20+Attr 12
;                            段基址,       段界限, 属性
LABEL_GDT:           Descriptor 0,              0, 0     		; 空描述符        
LABEL_DESC_NORMAL:   Descriptor 0,         0ffffh, DA_DRW		; Normal 描述符 用于准备保护模式回到实模式，DA_表示描述符属性  DA_DRW	EQU	92h	; 存在的可读写数据段属性值（见pm.imc 204行）
																; 因为这个过程需要加载一个合适的描述符选择子到有关段寄存器，以使对应段描述符高速缓冲寄存器中含有合适的段界限和属性
LABEL_DESC_PAGE_DIR: Descriptor PageDirBase, 4095, DA_DRW       ;Page Directory   页目录表描述符  将页目录开始地址PageDirBase装入段基址  段界限4K 默认段界限粒度为一个字节   为可读写数据段
LABEL_DESC_PAGE_TBL: Descriptor PageTblBase, 1023, DA_DRW|DA_LIMIT_4K;Page Tables 页表描述符      ;将页表开始地址PageTblBase装入段基址  段界限1K 对应1K个页表 
                                                                                                  ;DA_LIMIT_4K	EQU	8000h	          ; 段界限粒度为 4K 字节   ; 所有页表对应4MB
																								  ;12位（传的16位中中间有4位无用）属性中    G表示段界限粒度   G=1 段界限粒度为4KB
LABEL_DESC_CODE32:   Descriptor 0, SegCode32Len-1, DA_C+DA_32		; 32位非一致代码段描述符  ; SegCode32Len	equ	$ - LABEL_SEG_CODE32  表示32位代码段长度
																						; DA_C		EQU	98h	; 存在的只执行代码段属性值  （见pm.imc 206行）
                                                                                        ; DA_32	EQU	4000h	; 32 位段 （见pm.imc 193行）
																						;       D/B  在可执行代码段中 D=1在默认情况下指令使用32位地址及32位或8位操作数
LABEL_DESC_CODE16:   Descriptor 0,         0ffffh, DA_C				; 16位非一致代码段描述符      ;16位代码段有64K段界限 代码段属性为只执行
LABEL_DESC_DATA:     Descriptor 0,      DataLen-1, DA_DRW			; 数据段描述符          ; DataLen		equ	$ - LABEL_DATA   表示数据段长度
																						; DA_DRW		EQU	92h	; 存在的可读写数据段属性值（见pm.imc 204行）
LABEL_DESC_STACK:    Descriptor 0,     TopOfStack, DA_DRWA + DA_32	; 32 位堆栈段描述符  保护模式中用到  TopOfStack表示512个字节的大小
																						; DA_DRWA		EQU	93h	; 存在的已访问可读写数据段类型值（见pm.imc 205行）
LABEL_DESC_VIDEO:    Descriptor 0B8000h,   0ffffh, DA_DRW			; 该描述符记录显存首地址   ;直接装入段基址  ; B8000~BFFFF: CGA彩色字符模式显存空间
; GDT 结束

GdtLen		equ	$ - LABEL_GDT	; GDT长度  $表示当前行被汇编后的地址  这里其实相当于$ - $$
GdtPtr		dw	GdtLen - 1		; GDT界限  即所有描述符所占大小 使用一个字 即两个字节存放
		    dd	0		    	; GDT基地址   使用双字 即四个字节存放（32位），GdtPtr共6字节  用于lgdt指令

; GDT 选择子   作为索引，在GDT表中选择对应描述符
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT   ;Normal 描述符的选择子
SelectorPageDir		equ	LABEL_DESC_PAGE_DIR	- LABEL_GDT   ;页目录表描述符的选择子
SelectorPageTbl		equ	LABEL_DESC_PAGE_TBL	- LABEL_GDT   ;页表描述符的选择子
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT   ;32位代码段描述符的选择子
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT   ;16位代码段描述符的选择子
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT   ;数据段描述符的选择子
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT   ;32位堆栈段描述符的选择子
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT   ;显存描述符
; END of [SECTION .gdt]

[SECTION .data1]	 ; 数据段
ALIGN	32   ;该段的数据32字节对齐
[BITS	32]  ;告诉汇编器接下来的程序要被当作32位的程序来运行，操作数大小不显式指明的时候可以用
LABEL_DATA:
SPValueInRealMode	dw	0  ;用于保存实模式下sp所指的值，在保护模式返回实模式时用到，正好为一个字（16bit）
; 字符串
PMMessage:		db	"In Protect Mode now. ^-^", 0	; 进入保护模式后显示此字符串
OffsetPMMessage		equ	PMMessage - $$              ; 字符串在数据段中的偏移 $$表示当前节（section）开始处的地址  相当于PMMessage - LABEL_DATA 保护模式使用这个偏移
DataLen			equ	$ - LABEL_DATA                  ; 数据段长度
; END of [SECTION .data1]


; 全局堆栈段
[SECTION .gs]     ;堆栈段
ALIGN	32        ;该段的指令或数据32字节对齐
[BITS	32]       ;告诉汇编器接下来的程序要被当作32位的程序来运行
LABEL_STACK:
	times 512 db 0  ;512个字节的堆栈

TopOfStack	equ	$ - LABEL_STACK - 1  ; TopOfStack表示512个字节的堆栈大小

; END of [SECTION .gs]


[SECTION .s16]    ;16位代码段
[BITS	16]       :告诉汇编器接下来的程序要被当作16位的程序来运行
LABEL_BEGIN:      ;从第13行跳入 ,程序从这里开始
	mov	ax, cs
	mov	ds, ax    ;利用ax寄存器将cs的值赋给ds、es、ss
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h  ;sp指针指向0100h处

	mov	[LABEL_GO_BACK_TO_REAL+3], ax  ;为回到实模式这个跳转指令指定正确的段地址，实模式长跳转指令的第一个字节为OEAh，第2、3个字节为Offset，第4、5个字节为Segment
	                                   ;LABEL_GO_BACK_TO_REAL +3 正好是Segment的地址，此时ax正好是实模式下cs，
									   ;等到保护模式回到实模式中 jmp 0：LABEL_REAL_ENTRY ，段地址就不会是0，而是实模式下的cs
	mov	[SPValueInRealMode], sp        ;记录实模式下sp指针位置

	; 初始化 16 位代码段描述符   定义gdt时 LABEL_DESC_CODE16 描述符的段基址还是0 应换成对应16位代码段物理地址。而段界限和属性早已指定
	mov	ax, cs
	movzx	eax, ax   ;将cs（16位）扩充为32位并放入eax寄存器
	shl	eax, 4        ;eax左移四位  ，即段地址×16
	add	eax, LABEL_SEG_CODE16   ;结合前一句  是实模式下物理地址的计算方式：段地址×16+偏移  LABEL_SEG_CODE16正好是偏移
	mov	word [LABEL_DESC_CODE16 + 2], ax   ;由于段描述符中段基址存放不连续，同时为了直接使用ax寄存器，一共三部分进行段基址的存放，具体可对应pm.inc 75行中描述符的图示二
	shr	eax, 16                            ;已经放置了后16位（0~15），后边是处理前边的16位
	mov	byte [LABEL_DESC_CODE16 + 4], al   ;段基址16~23
	mov	byte [LABEL_DESC_CODE16 + 7], ah   ;段基址24~31

	; 初始化 32 位代码段描述符   大致同16位代码段描述符的初始化，仅需将物理地址赋给描述符中相应位置，以下数据段和堆栈段初始化同理
	xor	eax, eax
	mov	ax, cs
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32          ;因为仍是实模式下，所以也是采用实模式计算物理地址方法计算32位代码段的物理地址
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah

	; 初始化数据段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	; 初始化堆栈段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK
	mov	word [LABEL_DESC_STACK + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah

	; 为加载 GDTR 作准备
	xor	eax, eax   ;清空eax，也会使CF标志位清0
	mov	ax, ds     ;将ds赋给ax寄存器
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt 基地址   此时eax是gdt的物理地址
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt 基地址       GdtPtr+2 对应gdt的段基址

	; 加载 GDTR
	lgdt	[GdtPtr]  ;将GdtPtr指示的6字节加载到寄存器gdtr（前16位为界限，后32位为基地址）

	; 关中断
	cli  ;之所以关中断，是因为保护模式下中断处理的机制是不同的，不关掉中断将会出现错误。

	; 打开地址线A20  ;打开A20是由于历史问题防止偏移超出1M时回卷（wrap）。80286为了满足向上兼容，就选择了使用8042键盘控制器来控制第20个（从0开始数）地址位，这就是A20地址线
	in	al, 92h    ;通过操作端口92h来实现A20的打开
	or	al, 00000010b
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1    ;cr0的第0位是PE位，PE =1时CPU运行于保护模式
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 SelectorCode32:0  处（对应第181行）
                                ; SelectorCode32最终对应LABEL_DESC_CODE32描述符中的段基址，即标号LABEL_SEG_CODE32的物理地址。
								; 这个jmp在16位段中，目标地址却是32位的，比较特殊。所以 dword这里不能省略，dword应该加在偏移前，但NASM允许加在整个地址之前。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里，程序最后运行的一部分
	mov	ax, cs          ; 重新设置各个段寄存器的值
	mov	ds, ax
	mov	es, ax
	mov	ss, ax

	mov	sp, [SPValueInRealMode] ; 恢复sp的值

	in	al, 92h		; ┓
	and	al, 11111101b	; ┣ 关闭 A20 地址线 ;
	out	92h, al		; ┛

	sti			; 开中断

	mov	ax, 4c00h	; ┓
	int	21h		; ┛回到 DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]

LABEL_SEG_CODE32:
	call	SetupPaging  ;执行启动分页机制函数（第215行）

	mov	ax, SelectorData    ; 结合下一句为ds指向数据段
	mov	ds, ax			    ; 数据段选择子赋给ds
	mov	ax, SelectorVideo   ; 结合下一句为gs指向视频段
	mov	gs, ax			    ; 视频段选择子赋给gs

	mov	ax, SelectorStack   ; 结合下一句为ss指向堆栈段
	mov	ss, ax			    ; 堆栈段选择子赋给ss

	mov	esp, TopOfStack     ; 堆栈指针指向栈顶


	; 下面显示一个字符串
	mov	ah, 0Ch			; 0000: 黑底    1100: 红字
	xor	esi, esi
	xor	edi, edi
	mov	esi, OffsetPMMessage	; esi指向源数据偏移地址
	mov	edi, (80 * 10 + 0) * 2	; edi指向目的数据偏移。初始光标位置为屏幕第 10 行, 第 0 列。edi在接下来的循环中始终指向要显示的下一个字符的位置
	cld                         ; 清方向标志 使DF=0，在lodsb中会使得si增加1
.1:
	lodsb                       ; 从由DS：SI所指向的内存单元开始，去一个字节进入al
	test	al, al              ; 测试al寄存器是否为空，为空代表字符串输出结束
	jz	.2                      ; al为空，跳出输出字符串的循环
	mov	[gs:edi], ax            ; 将字符显示在屏幕对应位置
	add	edi, 2					; 光标指向下一个字符位置
	jmp	.1						; 继续循环
.2:	; 显示完毕

	; 到此停止
	jmp	SelectorCode16:0        ; 返回实模式，由于不能从32位代码段返回实模式，所以需要这样一个16位代码段

; 启动分页机制 --------------------------------------------------------------
SetupPaging:
	; 为简化处理, 所有线性地址对应相等的物理地址.

	; 首先初始化页目录
	mov	ax, SelectorPageDir	; 该页目录表选择子对应段首地址为 PageDirBase  2M处
	mov	es, ax              ; 将段寄存器es对应页目录表段选择子
	mov	ecx, 1024		    ; 共 1K 个表项，通过ecx保存个数用于页目录初始化循环
	xor	edi, edi            ; 让edi等于0，于是es:edi 指向页目录表的开始
	xor	eax, eax            ; eax清零
	mov	eax, PageTblBase | PG_P  | PG_USU | PG_RWW  ;这时eax对应第一个页目录表项（PDE），第一个页目录表项对应第一个页表，其页表首地址为PageTblBase
	                                                                                     ;属性为存在的可读可写可执行的用户级别页表
														; PG_P		    EQU	1	; 页存在属性位 （pm.inc 254行）
                                                        ; PG_USU		EQU	4	; U/S 属性位值, 用户级  （pm.inc 258行）
													    ; PG_RWW		EQU	2	; R/W 属性位值, 读/写/执行  （pm.inc 256行）
.1:
	stosd               ; stosd:将eax的内容复制到edi的内存空间，复制四个字节，并将edi加4个字节
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的. 由于每个表项大小为4k，eax增加4K，正好对应下一个页表首地址
	loop	.1          ; 进行循环，将所有页表连续排列在以PageTblBase为首地址的4MB（4096×1024）空间中

	; 再初始化所有页表 (1K 个, 4M 内存空间)
	mov	ax, SelectorPageTbl	; 整个页表空间选择子对应段首地址为 PageTblBase 2M+4K处
	mov	es, ax              ; 将段寄存器es对应整个页表空间选择子
	mov	ecx, 1024 * 1024	; 共 1M 个页表项, 也即有 1M 个页，循环1024×1024次
	xor	edi, edi			; edi为0，es：edi指向页表段首地址
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW  ;这时eax对应第一个页表项（PTE），第一个页表项对应第一个页框，该首地址为0 。属性为存在的可读可写可执行的用户级别页
																	 ;由于第0个页表的第0页对应物理地址0，所以线性地址0~0FFFh对应物理地址0~0FFFh 
																	 ;第一页线性地址等于物理地址，由于页表连续存储，之后4G空间亦符合线性地址等于物理地址
.2:                         ; 循环以初始化所有页表的所有页表项
	stosd 
	add	eax, 4096		; 每一页指向 4K 的空间，每次循环eax增加4K，对应下一个页的地址
	loop	.2

	mov	eax, PageDirBase　　; 将页目录表物理地址赋给eax，用于下一句加载进cr3寄存器以便正式启动分页机制
	mov	cr3, eax            ; cr3是页目录基址寄存器，保存页目录表的物理地址，页目录表总是放在以4K字节为单位的存储器边界上，因此，它的地址的低12位总为0，不起作用，即使写上内容，也不会被理会。
	mov	eax, cr0            ; 分页机制是否生效的开关位于cr0的最高位PG位，如果PG=1，则分页机制生效。
	or	eax, 80000000h      ; 操作数为2的31次方，与eax中的值做或运算，将eax的第31置为1，而CR0寄存器的PG位恰好为第31位，结合下一句，可将PG置1
	mov	cr0, eax
	jmp	short .3            ; 短跳转指令，
.3:
	nop                     ; 空操作指令，只使程序计数器PC加1，占用一个机器周期

	ret                     ; 弹栈到ip，回到32位代码段中
; 分页机制启动完毕 ----------------------------------------------------------

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段. 由 32 位代码段跳入, 跳出后到实模式
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov	ax, SelectorNormal  ; 加载Normal描述符对应选择子到ds、es、ss等段寄存器
	mov	ds, ax              ; 因为要使得对应段描述符高速缓存寄存器中含有合适的段界限和属性
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax

	mov	eax, cr0            
	and	eax, 7FFFFFFEh		; 同时令cr0寄存器的 PE=0, PG=0
	mov	cr0, eax

LABEL_GO_BACK_TO_REAL:      ; 正式返回实模式的入口
	jmp	0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值（第85行）

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
