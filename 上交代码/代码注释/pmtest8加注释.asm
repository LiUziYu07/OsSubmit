; ==========================================
; pmtest8.asm
; 编译方法：nasm pmtest8.asm -o pmtest8.com
; ==========================================

%include	"pm.inc"	; 常量, 宏, 以及一些说明

PageDirBase0		equ	200000h	; 第一个页目录表开始地址:	2M
PageTblBase0		equ	201000h	; 第一个页表空间开始地址:   2M +  4K
PageDirBase1		equ	210000h	; 第二个页目录表开始地址:	2M + 64K (可以看出，第一个页目录中不会超过15个页表)
PageTblBase1		equ	211000h	; 第二个页表空间开始地址:	2M + 64K + 4K

LinearAddrDemo	equ	00401000h   ; LinearAddrDemo可以看作线性地址
ProcFoo		    equ	00401000h   ; ProcFoo在内存中物理地址
ProcBar		    equ	00501000h	; ProcBar在内存中物理地址
ProcPagingDemo	equ	00301000h   ; ProcPagingDemo在内存中物理地址

org	0100h  ;告诉编译器，这段程序要被加载到内存偏移地址0x0100h，段的前0100h（256字节）是留给程序段前缀PSP的。这是操作系统DOS的概念
		   ;对于一些引导程序，这些程序将被BIOS装载到内存中的指定地方，通常为0000:7c00，DOS是将COM文件的代码装载到CS:0100处
	jmp	LABEL_BEGIN   ;跳转到LABEL_BEGIN处开始执行，此时还处于实模式中

[SECTION .gdt]   ;将以下代码或数据分配到gdt段，SECTION默认4字节对齐的：
; GDT			 ;Descriptor是在pm.inc（267~278）中定义的描述符宏  Descriptor Base, Limit, Attr   共8字节64位 Base 32+Limit 20+Attr 12
;                           段基址,       段界限, 属性
LABEL_GDT:          Descriptor 0,              0, 0         		; 空描述符
LABEL_DESC_NORMAL:  Descriptor 0,         0ffffh, DA_DRW            ; Normal 描述符   用于准备保护模式回到实模式。
																		 ; DA_表示描述符属性  DA_DRW	EQU	92h	; 存在的可读写数据段属性值（见pm.imc 204行）
LABEL_DESC_FLAT_C:  Descriptor 0,        0fffffh, DA_CR|DA_32|DA_LIMIT_4K  ; FLAT段存放两套页目录和页表空间，该描述符会指向FLAT段，但属性主要为可执行，长度4G
																				; DA_CR		EQU	9Ah	; 存在的可执行可读代码段属性值 （见pm.inc 207行）
																				; DA_32		EQU	4000h	; 32 位段 （见pm.inc 193行）
																				; DA_LIMIT_4K	EQU	8000h	; 段界限粒度为 4K 字节 （见pm.inc 194行）段长度是(0xFFFFF + 1) × 0x1000 -1 = 0xFFFFFFFF；
LABEL_DESC_FLAT_RW: Descriptor 0,        0fffffh, DA_DRW|DA_LIMIT_4K       ; 该描述符同样指向FLAT段，但属性主要为可读写,长度4G
																			    ; DA_DRW		EQU	92h	; 存在的可读写数据段属性值（见pm.inc 204行）
																				; DA_LIMIT_4K	EQU	8000h	; 段界限粒度为 4K 字节 （见pm.inc 194行）
LABEL_DESC_CODE32:  Descriptor 0, SegCode32Len-1, DA_CR|DA_32       ; 32位非一致代码段描述符  
																		; SegCode32Len	equ	$ - LABEL_SEG_CODE32  表示32位代码段长度
																		; DA_C		EQU	98h	; 存在的只执行代码段属性值  （见pm.imc 206行）
                                                                        ; DA_32  	EQU	4000h	; 32 位段 （见pm.imc 193行）
																		; D/B  在可执行代码段中 D=1在默认情况下指令使用32位地址及32位或8位操作数
LABEL_DESC_CODE16:  Descriptor 0,         0ffffh, DA_C              ; 16位非一致代码段描述符      ;16位代码段有64K段界限 代码段属性为只执行
LABEL_DESC_DATA:    Descriptor 0,      DataLen-1, DA_DRW            ; 数据段描述符         
																		; DataLen		equ	$ - LABEL_DATA   表示数据段长度
																		; DA_DRW		EQU	92h	; 存在的可读写数据段属性值（见pm.imc 204行）
LABEL_DESC_STACK:   Descriptor 0,     TopOfStack, DA_DRWA|DA_32     ; 32 位堆栈段描述符  保护模式中用到  TopOfStack表示512个字节的大小
																		; DA_DRWA		EQU	93h	; 存在的已访问可读写数据段类型值（见pm.imc 205行）
LABEL_DESC_VIDEO:   Descriptor 0B8000h,   0ffffh, DA_DRW                ; 该描述符记录显存首地址   ;直接装入段基址  ; B8000~BFFFF: CGA彩色字符模式显存空间
; GDT 结束

GdtLen		equ	$ - LABEL_GDT	; GDT长度  $表示当前行被汇编后的地址  这里其实相当于$ - $$
GdtPtr		dw	GdtLen - 1	; GDT界限      即所有描述符所占大小 使用一个字 即两个字节存放
		    dd	0		    ; GDT基地址    使用双字 即四个字节存放（32位），GdtPtr共6字节  用于lgdt指令

; GDT 选择子   作为索引，在GDT表中选择对应描述符
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT   ;Normal 描述符的选择子
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT   ;FLAT段 可执行描述符的选择子
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT   ;FLAT段 可读写描述符的选择子
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT   ;32位代码段描述符的选择子
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT   ;16位代码段描述符的选择子
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT   ;数据段描述符的选择子
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT   ;32位堆栈段描述符的选择子
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT   ;显存描述符
; END of [SECTION .gdt]

[SECTION .data1]	 ; 数据段
ALIGN	32  ;该段的数据32字节对齐
[BITS	32]  ;告诉汇编器接下来的程序要被当作32位的程序来运行，操作数大小不显式指明的时候可以用
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串作为打印内存信息时表格头
_szRAMSize			db	"RAM size:", 0  ; 作为显示机器的内存空间的提示符
_szReturn			db	0Ah, 0	; 用于换行
; 变量
_wSPValueInRealMode		dw	0   ; 用于保存实模式下sp所指的值，在保护模式返回实模式时用到，正好为一个字（16bit）
_dwMCRNumber:			dd	0	; 用来记录地址范围描述符结构的个数
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。显示时光标初始位置
_dwMemSize:			dd	0                   ; 机器的内存空间，其值在“RAM size”字符串后显示
_ARDStruct:			; Address Range Descriptor Structure地址范围描述符结构  我们利用中断15h得知机器有多少内存，其中涉及ARDS结构 可以看出该结构体共20字节
	_dwBaseAddrLow:		dd	0	; BaseAddrLow   偏移0  代表基地址的低32位 
	_dwBaseAddrHigh:	dd	0	; BaseAddrHigh  偏移4  代表基地址的高32位
	_dwLengthLow:		dd	0	; LengthLow     偏移8  长度（字节）的低32位
	_dwLengthHigh:		dd	0	; LengthHigh    偏移12 长度（字节）的高32位
	_dwType:		    dd	0	; Type			偏移16 这个地址范围的地址类型
_PageTableNumber		dd	0	; 页表个数

_MemChkBuf:	times	256	db	0	; ax=0E820h时调用int 15h得到的不仅仅是内存的大小，还包括对不同内存段的一些描述。描述被保存在一个缓冲区中。
								; MemChkBuf就是这样一块256字节的缓冲区，最多可以存放12个20字节的结构体。
; 保护模式下使用这些符号        ; 程序是在实模式下编译的，地址只适用于实模式，在保护模式下，数据的地址应该是其相对于段基址的偏移。
szPMMessage		equ	_szPMMessage	- $$			; 字符串在数据段中的偏移 $$表示当前节（section）开始处的地址  相当于_szPMMessage - LABEL_DATA 保护模式使用这个偏移
szMemChkTitle		equ	_szMemChkTitle	- $$		; 以下与实模式下使用的符号一一对应，仅是记录了在数据段中的偏移，以便保护模式使用对应变量或字符串
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
PageTableNumber		equ	_PageTableNumber- $$

DataLen			equ	$ - LABEL_DATA		 ; 数据段长度
; END of [SECTION .data1]


; 全局堆栈段
[SECTION .gs]		; 堆栈段
ALIGN	32			; 该段的指令或数据32字节对齐
[BITS	32]		    ; 告诉汇编器接下来的程序要被当作32位的程序来运行
LABEL_STACK:
	times 512 db 0	; 512个字节的堆栈

TopOfStack	equ	$ - LABEL_STACK - 1	; TopOfStack表示512个字节的堆栈大小

; END of [SECTION .gs]


[SECTION .s16]		; 16位代码段
[BITS	16]			; 告诉汇编器接下来的程序要被当作16位的程序来运行
LABEL_BEGIN:		; 从第20行跳入 ,程序从这里开始正式运行
	mov	ax, cs
	mov	ds, ax		; 利用ax寄存器将cs的值赋给ds、es、ss
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h	; sp指针指向0100h处

	mov	[LABEL_GO_BACK_TO_REAL+3], ax  ; 为回到实模式这个跳转指令指定正确的段地址，实模式长跳转指令的第一个字节为OEAh，第2、3个字节为Offset，第4、5个字节为Segment
	                                   ; LABEL_GO_BACK_TO_REAL +3 正好是Segment的地址，此时ax正好是实模式下cs，
									   ; 等到保护模式回到实模式中 jmp 0：LABEL_REAL_ENTRY ，段地址就不会是0，而是实模式下的cs
	mov	[_wSPValueInRealMode], sp	   ; 记录实模式下sp指针位置，保护模式返回实模式时需要它恢复sp指针位置

	; 得到内存数	调用中断15h前，需要填充一些寄存器如：eax、ebx、es：di、ecx、edx，调用的结果存放在寄存器中如：CF、eax、es：di、ecx、ebx，具体内容见下面注释
	mov	ebx, 0					; ebx放置着“后续值(continuation value)”，第一次调用时ebx必须为0
	mov	di, _MemChkBuf			; es:di在调用前应指向一个ARDS，BIOS会填充此结构。经此句后，es：di直接指向缓冲区，在缓冲区中填充，填充一次(20字节)后，di+20（参见第146行）
								       ; es：di 中断调用返回的是ARDS指针，同输入值相同 
.loop:				; 在缓冲区中不断填充ARDS结构体
	mov	eax, 0E820h				; int 15h可完成许多工作，我们想要获取内存信息，需要将ax赋值为0E820h，中断返回成功时eax值为0534D4150h('SMAP') 否则eax为一个出错码
	mov	ecx, 20					; es：di所指向的ARDS大小，以字节为单位。无论es:di指向的结构如何设置，BIOS最多填充ecx个字节。不过通常无论ecx为多大，BIOS只填充20字节
	mov	edx, 0534D4150h			; 0534D4150h('SMAP') ，BIOS会使用此标志，对调用者将要请求的系统映像信息进行校验，这些信息会被BIOS放置到es：di所指向的结构中
	int	15h			; 执行 int 15h中断
	jc	LABEL_MEM_CHK_FAIL		; CF = 0 表示没有错误，否则存在错误。如果有错误，则跳转到对应函数，将已记录的地址范围描述符个数清零
	add	di, 20					; di+20 ，指向下一个ARDS区域等待填充
	inc	dword [_dwMCRNumber]	; 地址范围描述符个数加1
	cmp	ebx, 0					; ebx第一次调用时必须为0，当调用完之后，如果ebx为0，并且CF没有进位，就表示是最后一个地址范围描述符
	jne	.loop
	jmp	LABEL_MEM_CHK_OK		; 若正常运行，则会跳转到第153行，准备实模式进入保护模式的工作
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0 ; 对应int 15h有错误的情况，会将记录的地址范围描述符个数清零
LABEL_MEM_CHK_OK:			; 得到内存信息后，实模式进入保护模式的一系列工作

	; 初始化 16 位代码段描述符		定义gdt时 LABEL_DESC_CODE16 描述符的段基址还是0 应换成对应16位代码段物理地址。而段界限和属性早已指定
	mov	ax, cs				
	movzx	eax, ax			;将cs（16位）扩充为32位并放入eax寄存器
	shl	eax, 4				; eax左移四位  ，即段地址×16
	add	eax, LABEL_SEG_CODE16		; 结合前一句  是实模式下物理地址的计算方式：段地址×16+偏移  LABEL_SEG_CODE16正好是偏移
	mov	word [LABEL_DESC_CODE16 + 2], ax	; 由于段描述符中段基址存放不连续，同时为了直接使用ax寄存器，一共三部分进行段基址的存放，具体可对应pm.inc 75行中描述符的图示二
	shr	eax, 16								; 已经放置了后16位（0~15），后边是处理前边的16位
	mov	byte [LABEL_DESC_CODE16 + 4], al	; 段基址16~23
	mov	byte [LABEL_DESC_CODE16 + 7], ah	; 段基址24~31

	; 初始化 32 位代码段描述符		大致同16位代码段描述符的初始化，仅需将物理地址赋给描述符中相应位置，以下数据段和堆栈段初始化同理
	xor	eax, eax
	mov	ax, cs
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32				;仍是实模式下，也是采用实模式计算物理地址方法计算32位代码段的物理地址
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
	xor	eax, eax		; 清空eax，也会使CF标志位清0
	mov	ax, ds			; 将ds赋给ax寄存器
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt 基地址   此时eax是gdt的物理地址
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt 基地址	 GdtPtr+2 对应gdt的段基址

	; 加载 GDTR
	lgdt	[GdtPtr]		; 将GdtPtr指示的6字节加载到寄存器gdtr（前16位为界限，后32位为基地址）

	; 关中断
	cli				; 之所以关中断，是因为保护模式下中断处理的机制是不同的，不关掉中断将会出现错误。

	; 打开地址线A20	; 打开A20是由于历史问题防止偏移超出1M时回卷（wrap）。80286为了满足向上兼容，就选择了使用8042键盘控制器来控制第20个（从0开始数）地址位，这就是A20地址线
	in	al, 92h			
	or	al, 00000010b	; 通过操作端口92h来实现A20的打开
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1			; cr0的第0位是PE位，PE =1时CPU运行于保护模式
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 SelectorCode32:0  处（对应第246行）
								; SelectorCode32最终对应LABEL_DESC_CODE32描述符中的段基址，即标号LABEL_SEG_CODE32的物理地址。
								; 这个jmp在16位段中，目标地址却是32位的，比较特殊。所以 dword这里不能省略，dword应该加在偏移前，但NASM允许加在整个地址之前。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里，程序最后运行的一部分
	mov	ax, cs		; 重新设置各个段寄存器的值
	mov	ds, ax
	mov	es, ax
	mov	ss, ax

	mov	sp, [_wSPValueInRealMode]	; 恢复实模式下sp的值

	in	al, 92h		; ┓
	and	al, 11111101b	; ┣ 关闭 A20 地址线
	out	92h, al		; ┛

	sti			; 开中断

	mov	ax, 4c00h	; ┓
	int	21h		; ┛回到 DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorData	 ; 结合下两句为ds、es指向数据段
	mov	ds, ax				 ; 数据段选择子赋给ds
	mov	es, ax				 ; 数据段选择子赋给es
	mov	ax, SelectorVideo	 ; 结合下一句为gs指向视频段
	mov	gs, ax				 ; 视频段选择子赋给gs

	mov	ax, SelectorStack	 ; 结合下一句为ss指向堆栈段
	mov	ss, ax				 ; 堆栈段选择子赋给ss

	mov	esp, TopOfStack		 ; 堆栈指针指向栈顶


	; 下面三行是显示一个字符串
	push	szPMMessage		 ; 将字符串首地址压栈，在DispStr函数调用中会用到
	call	DispStr			 ; 是一个显示字符串函数（见lib.inc 74行）函数中[ebp+8]正好对应上一步字符串首地址，
																; 因为cs和ip压栈占了4个字节，函数中push ebp后ebp记录esp，ebp占4个字节，
																; 故[ebp+8]内容正好为字符串首地址，赋给esi有相应显示操作
																; 用到的参数还有dwDisPos用来指定光标位置，并且dwDisPos每次打印前都有所调整
	add	esp, 4				 ; 因为调用前压入了一个32位的地址，所以要自己平衡一下堆栈的使用情况
	; 下面三行是打印表头
	push	szMemChkTitle	 ; 该三行为打印表头，用来帮助理解显示内存信息
	call	DispStr
	add	esp, 4				 ; 同上一个字符串打印操作

	call	DispMemSize		 ; 显示内存信息（第454行）

	call	PagingDemo		 ; 演示改变页目录的效果（第331行）该程序核心部分

	; 到此停止
	jmp	SelectorCode16:0	 ; 准备从保护模式回到实模式，由于不能从32位代码段返回实模式，所以需要这样一个16位代码段

; 启动分页机制 --------------------------------------------------------------
SetupPaging:				 ; 该函数只是初始化了第一个页目录表和对应页表空间
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize] ; 内存容量 -> eax
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx				; 内存大小除以4MB，得到应初始化的页表项PDE的个数，也是页表的个数
	mov	ecx, eax	    ; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx	; edx 保存284行div 的余数
	jz	.no_remainder	; 没有余数则直接进行页目录表和页表的初始化
	inc	ecx				; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	; 暂存页表个数

	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存碎片.

	; 首先初始化页目录
	mov	ax, SelectorFlatRW	; FLAT段选择子对应段首地址0处
	mov	es, ax				; 将段寄存器es对应FLAT段选择子
	mov	edi, PageDirBase0	; edi赋为 PageDirBase0 于是es:edi 指向第一个页目录表的开始
	xor	eax, eax			; eax清零
	mov	eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW		;这时eax对应第一个页目录的第一个表项（PDE），第一个页目录的第一个表项对应第一个页表，其页表首地址为PageTblBase0
	                                                                                     ;属性为存在的可读可写可执行的用户级别页表
														; PG_P		    EQU	1	; 页存在属性位 （pm.inc 254行）
                                                        ; PG_USU		EQU	4	; U/S 属性位值, 用户级  （pm.inc 258行）
													    ; PG_RWW		EQU	2	; R/W 属性位值, 读/写/执行  （pm.inc 256行）
.1:
	stosd				; stosd:将eax的内容复制到edi的内存空间，复制四个字节，并将edi加4个字节
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的。eax增加4K，对应下一个页表首地址
	loop	.1			; 进行循环，通过页目录表的记录，将所有页表连续排列在以PageTblBase0为首地址的60KB（自己划分的）空间中（可能用不完）

	; 再初始化所有页表（只分配了60KB空间用来存放）
	mov	eax, [PageTableNumber]	; 页表个数 ->eax
	mov	ebx, 1024				; 每个页表 1024 个页表项（PTE）
	mul	ebx						; 页表个数 * 1024 ->ebx
	mov	ecx, eax				; 所有PTE个数 = 页表个数 * 1024 = 初始化页表循环次数
	mov	edi, PageTblBase0	    ; 经此句 es:edi 指向第一个页目录对应的页表空间的开始
	xor	eax, eax				; eax清零
	mov	eax, PG_P  | PG_USU | PG_RWW	;这时eax对应第一个页表项（PTE），第一个页表项对应第一个页框，该首地址为0 。属性为存在的可读可写可执行的用户级别页
																	 ;由于第0个页表的第0页对应物理地址0，所以线性地址0~0FFFh对应物理地址0~0FFFh 
																	 ;第一页线性地址等于物理地址，由于页表连续存储，之后4G空间亦符合线性地址等于物理地址
.2:					 ; 循环以初始化所有页表的所有页表项
	stosd				; 填写一个PTE，并将edi加4
	add	eax, 4096		; 每一页指向 4K 的空间，每次循环eax增加4K，对应下一个页的起始地址
	loop	.2

	mov	eax, PageDirBase0	; 将第一个页目录表物理地址赋给eax，用于下一句加载进cr3寄存器以便正式启动分页机制
	mov	cr3, eax			; cr3是页目录基址寄存器，保存页目录表的物理地址，页目录表总是放在以4K字节为单位的存储器边界上，因此，它的地址的低12位总为0，不起作用，即使写上内容，也不会被理会。
	mov	eax, cr0			; 分页机制是否生效的开关位于cr0的最高位PG位，如果PG=1，则分页机制生效。
	or	eax, 80000000h		; 结合下一句，可将PG置1
	mov	cr0, eax
	jmp	short .3
.3:
	nop						; 空操作指令，只使程序计数器PC加1，占用一个机器周期

	ret						; 弹栈到ip，回到PagingDemo函数运行（368行）
; 分页机制启动完毕 ----------------------------------------------------------


; 测试分页机制 --------------------------------------------------------------
PagingDemo:          ; 运行时从第273行进入
	mov	ax, cs		 ; 这段是为MemCpy函数做准备
	mov	ds, ax		 ; cs赋给ds ，即源数据放在ds段中
	mov	ax, SelectorFlatRW
	mov	es, ax		 ; 对应存放页目录页表空间的段选择子赋给es， 即目的在es段中

	push	LenFoo	 	; void* MemCpy(void* es:pDest, void* ds:pSrc, int iSize);   此处的压栈正好对应MemCpy所需参数
	push	OffsetFoo   ; LenFoo -- iSize    OffsetFoo -- ds：pSrc   ProcFoo -- es：pDest
	push	ProcFoo		; OffsetFoo是源数据  ProcFoo是目标物理地址
	call	MemCpy		; 执行MemCpy 将foo函数代码放在指定的内存物理地址00401000h 处
	add	esp, 12			; 平衡堆栈

	push	LenBar		; 与foo函数的MemCpy同理
	push	OffsetBar
	push	ProcBar
	call	MemCpy		; 执行MemCpy 将bar函数代码放在指定的内存物理地址00501000h处
	add	esp, 12

	push	LenPagingDemoAll
	push	OffsetPagingDemoProc
	push	ProcPagingDemo
	call	MemCpy		; 执行MemCpy 将PagingDemoProc函数代码放在指定的内存物理地址00301000h处
	add	esp, 12

	mov	ax, SelectorData ; 使用完毕ds、es完成内存拷贝，恢复ds、es仍指向数据段
	mov	ds, ax			; 数据段选择子
	mov	es, ax

	call	SetupPaging		; 启动分页（跳转到279行开始运行）

	call	SelectorFlatC:ProcPagingDemo	; 会运行 PagingDemoProc，其实也就是call LinearAddrDemo（00401000h） ，此时按照第一个页目录表执行，线性地址等于物理地址，即运行内存中00401000h对应位置的代码
											; 即运行 foo函数的代码，结果为输出‘Foo’字样
	call	PSwitch			; 切换页目录，改变地址映射关系 （378行）
	call	SelectorFlatC:ProcPagingDemo	;  会运行 PagingDemoProc，call LinearAddrDemo（00401000h） ，此时按照第二个页目录表执行，线性地址仍是00401000h，但由于第二个页目录中页表的修改，
											; 该线性地址映射的物理地址不再是00401000h，而是00501000h，即运行 bar函数的代码，结果为输出‘Bar’字样

	ret						; 运行结束，回到32位代码段
; ---------------------------------------------------------------------------


; 切换页表 ------------------------------------------------------------------
PSwitch:
	; 初始化第二个页目录
	mov	ax, SelectorFlatRW	; FLAT段选择子对应段首地址0处
	mov	es, ax				; 将段寄存器es对应FLAT段选择子
	mov	edi, PageDirBase1	; edi赋为 PageDirBase1 于是es:edi 指向第二个页目录表的开始
	xor	eax, eax			; eax清零
	mov	eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW  ;这时eax对应第二个页目录的第一个表项（PDE），第二个页目录的第一个表项对应第一个页表，其页表首地址为PageTblBase1
	mov	ecx, [PageTableNumber] ; 初始化第一个页目录时，已经计算并存储了多少共多少PDE或页表，直接赋给ecx参与循环
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中仍是连续的.eax增加4K，对应下一个页表首地址
	loop	.1

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数 ->eax
	mov	ebx, 1024			; 每个页表 1024 个 PTE
	mul	ebx	
	mov	ecx, eax			; 所有PTE个数 = 页表个数 * 1024 = 初始化页表循环次数
	mov	edi, PageTblBase1	; 经此句 es:edi 指向第二个页目录对应的页表空间的开始
	xor	eax, eax			; eax清零
	mov	eax, PG_P  | PG_USU | PG_RWW ;这时eax对应第一个页表项（PTE），第一个页表项对应第一个页框，该首地址为0 。
.2:				 ; 循环以初始化所有页表的所有页表项
	stosd				; 填写一个PTE，并将edi加4	
	add	eax, 4096		; 每一页指向 4K 的空间，每次循环eax增加4K，对应下一个页的起始地址
	loop	.2

	; 在此假设内存是大于 8M 的，以下是修改第二个页目录表中LinearAddrDemo作为线性地址00401000h所在页表的对应页表项内的物理地址，进而在寻址时，线性地址会映射到不同于第一次的物理地址
	mov	eax, LinearAddrDemo ; 00401000h ->eax
	shr	eax, 22				; 右移22位，对应页目录号，即寻找是哪个页表
	mov	ebx, 4096			
	mul	ebx					; 因为页表是连续4KB存储的，所以此时eax是00401000h对应页表相对于页表空间起始地址的偏移
	mov	ecx, eax			; ecx记录对应页表相对于页表空间起始地址的偏移
	mov	eax, LinearAddrDemo	; 重新00401000h ->eax，继续运算
	shr	eax, 12				; 右移12位，剩余页目录号+页号
	and	eax, 03FFh	; 1111111111b (10 bits)		; 屏蔽了页目录号，保留了10位的页号
	mov	ebx, 4				
	mul	ebx					; 页表中每个页表项占4个字节，所以此时eax是00401000h对应页表项相对于所在页表的偏移
	add	eax, ecx			
	add	eax, PageTblBase1	; 经此句，eax = 第二个页表空间起始地址+00401000h所在页表相对于页表空间起始地址的偏移+00401000h所在页表项相对于所在页表的偏移
	mov	dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW ;修改第二个页目录表中00401000h对应的物理地址为00501000h，该物理地址处代码为bar函数的代码

	mov	eax, PageDirBase1	; 将第二个页目录表物理地址赋给eax，用于下一句加载进cr3寄存器 
	mov	cr3, eax
	jmp	short .3
.3:
	nop						; 空操作指令，只使程序计数器PC加1，占用一个机器周期

	ret						; 弹栈到ip，回到PagingDemo函数运行（371行）
; ---------------------------------------------------------------------------



PagingDemoProc:			; 提供一个线性地址，用于两次地址映射的比较
OffsetPagingDemoProc	equ	PagingDemoProc - $$
	mov	eax, LinearAddrDemo
	call	eax  ; 即call 00401000h
	retf
LenPagingDemoAll	equ	$ - PagingDemoProc

foo:					; 第一次映射应使用的函数，打印‘Foo’字样
OffsetFoo		equ	foo - $$
	mov	ah, 0Ch			; 0000: 黑底    1100: 红字
	mov	al, 'F'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'o'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
	ret
LenFoo			equ	$ - foo

bar:					; 第二次映射应使用的函数，打印‘Bar’字样
OffsetBar		equ	bar - $$
	mov	ah, 0Ch			; 0000: 黑底    1100: 红字
	mov	al, 'B'
	mov	[gs:((80 * 18 + 0) * 2)], ax	; 屏幕第 18 行, 第 0 列。
	mov	al, 'a'
	mov	[gs:((80 * 18 + 1) * 2)], ax	; 屏幕第 18 行, 第 1 列。
	mov	al, 'r'
	mov	[gs:((80 * 18 + 2) * 2)], ax	; 屏幕第 18 行, 第 2 列。
	ret
LenBar			equ	$ - bar


; 显示内存信息 --------------------------------------------------------------
DispMemSize:
	push	esi			; 为了函数中正常使用寄存器而不丢失数据，进行压栈，函数结束后按顺序弹栈
	push	edi
	push	ecx

	mov	esi, MemChkBuf	; ds：esi指向缓冲区，通过循环从缓冲区中依次读出ARDS，以下保留原文注释的C代码
	mov	ecx, [dwMCRNumber]	;for(int i=0;i<[MCRNumber];i++) // 每次得到一个ARDS(Address Range Descriptor Structure)结构，ecx用来保存地址范围描述符的个数，也是循环次数
.loop:					;{
	mov	edx, 5			;	for(int j=0;j<5;j++)	// 每次得到一个ARDS中的成员，共5个成员，每个成员都是一个双字（4个字节），与第466行dword和470行对应
	mov	edi, ARDStruct		;	{			// 依次显示：BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
.1:					;
	push	dword [esi]		;	//DispInt 用来显示一个整型数，该句即传递函数所需参数，函数内将每个成员分成4个部分依次打印，最后加上h字符表示16进制
	call	DispInt			;		DispInt(MemChkBuf[j*4]); // 显示一个成员   （DispInt 函数具体参见lib.inc 第43行）
	pop	eax					;   //直接弹栈平衡堆栈  并将dword [esi]赋给eax，用于下一句stosd
	stosd				;		    ARDStruct[j*4] = MemChkBuf[j*4]; //从缓冲区中读出一个成员信息赋给ARDS结构  stosd:将eax的内容复制到edi的内存空间，复制四个字节，并将edi加4个字节
	add	esi, 4			;       //esi指向下一个成员
	dec	edx			;           //一个ARDS中待显示的成员数减一
	cmp	edx, 0			;       //是不是5个成员都打印了
	jnz	.1			;	        }       //5个成员未全部显示的话接着打印
	call	DispReturn		;	printf("\n");  //DispReturn 函数用来换行，会用到szReturn这个偏移地址（DispReturn 函数具体参见lib.inc第119行）
	cmp	dword [dwType], 1	;	if(Type == AddressRangeMemory) // AddressRangeMemory : 1, AddressRangeReserved : 2
	jne	.2		;	           {//Type是1，表示这个内存段可以被OS使用，Type是2，表示这个地址段正在被使用或被系统保留所以一定不要被OS使用，其他未定义必须被OS认为是 AddressRangeReserved
	mov	eax, [dwBaseAddrLow];   //如果经判断该内存段可以被OS使用，会通过下面两句计算内存大小
	add	eax, [dwLengthLow]	;   //基地址加长度，表示内存段大小，赋给eax 
	cmp	eax, [dwMemSize]	;		if(BaseAddrLow + LengthLow >= MemSize) //判断是否存在更大的可用内存地址
	jb	.2			;						//如果存在可用的更大内存地址，则修改已记录的内存容量
	mov	[dwMemSize], eax	;			MemSize = BaseAddrLow + LengthLow; //修改已记录的内存容量
.2:					;			}
	loop	.loop		  ;}//如果内存段不可用，直接查看下一个ARDS结构中的内存段情况
					;     //当ecx归0 即所有缓冲区中ARDS结构都已读完，进入接下来语句，准备输出内存容量RAM Size
	call	DispReturn		;printf("\n") 换行（参见lib.inc第119行）
	push	szRAMSize		;待打印字符串，DispStr函数的参数
	call	DispStr			;printf("RAM size:"); 显示字符串函数（见lib.inc 74行）
	add	esp, 4			;平衡堆栈，丢弃堆栈中已调用函数的参数
					;
	push	dword [dwMemSize]	;
	call	DispInt			;DispInt(MemSize);显示一个内存容量这个整型数（DispInt 函数具体参见lib.inc 第43行）
	add	esp, 4			;平衡堆栈

	pop	ecx		; 恢复现场
	pop	edi
	pop	esi
	ret			; 函数调用结束，返回到273行继续运行
; ---------------------------------------------------------------------------

%include	"lib.inc"	; 库函数

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段. 由 32 位代码段跳入, 跳出后到实模式
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov	ax, SelectorNormal	; 加载Normal描述符对应选择子到ds、es、ss等段寄存器，因为要使得对应段描述符高速缓存寄存器中含有合适的段界限和属性
	mov	ds, ax				; 重新设置ds寄存器
	mov	es, ax				; 重新设置es寄存器
	mov	fs, ax				; 重新设置fs寄存器
	mov	gs, ax				; 重新设置gs寄存器
	mov	ss, ax				; 重新设置ss寄存器

	mov	eax, cr0
	and     eax, 7FFFFFFEh          ; 同时令cr0寄存器的 PE=0, PG=0
	mov	cr0, eax

LABEL_GO_BACK_TO_REAL:		; 正式返回到实模式的入口
	jmp	0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值（第131行）

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
