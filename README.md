# NOP: A Strong Baseline for LoongArch Out-of-order Processor

> 本项目是第七届“龙芯杯”全国大学生计算机系统能力培养大赛 (NSCSCC 2023) 的参赛作品。项目成功地开发了一款基于龙芯架构 32 位精简版 (LoongArch32-Reduced, LA32R) 指令集的 CPU，命名为 NOP。NOP 属于乱序多发射微架构，整体基于 Tomasulo 动态调度算法的思路 + 重排序缓存实现，并实现了分支预测、指令/数据缓存、数据旁路、推测唤醒等特性。从功能的角度上，NOP 实现了基础的算术指令、分支指令、访存指令，支持精确异常处理、虚实地址转换与 LA32R 指令集规定的各类中断，通过使用 PMON 引导程序，NOP 可以稳定地启动Linux 操作系统。从性能上看，NOP 作为一款乱序多发射处理器，在大赛提供的 FPGA 实验平台与性能测试程序 / SoC 上达到了 107.69 MHz 的主频与 1.02 的 IPC。相比于基线 openLA500 处理器，NOP 核的整体加速比达到了 3.00，IPC 加速比达到了 1.402.

Code coming soon, stay tuned!