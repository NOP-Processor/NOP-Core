# NOP: A Strong Baseline for LoongArch Out-of-order Processor

[ [Video](https://www.bilibili.com/video/BV1qp4y1J7xj/?share_source=copy_web&vd_source=f8abf8e71d21b75d026544d7283c8603) ]

Authors: [Mingdao Liu](https://github.com/Btlmd/), [Huan-ang Gao](https://github.com/c7w/), [Bowen Wang](https://github.com/abmfy) and [Jiacheng Hua](https://github.com/fleurs03).


> 本项目是第七届“龙芯杯”全国大学生计算机系统能力培养大赛 (NSCSCC 2023) 的参赛作品。项目成功地开发了一款基于龙芯架构 32 位精简版 (LoongArch32-Reduced, LA32R) 指令集的 CPU，命名为 NOP。NOP 属于乱序多发射微架构，整体基于 Tomasulo 动态调度算法的思路 + 重排序缓存实现，并实现了分支预测、指令/数据缓存、数据旁路、推测唤醒等特性。从功能的角度上，NOP 实现了基础的算术指令、分支指令、访存指令，支持精确异常处理、虚实地址转换与 LA32R 指令集规定的各类中断，通过使用 PMON 引导程序，NOP 可以稳定地启动Linux 操作系统。从性能上看，NOP 作为一款乱序多发射处理器，在大赛提供的 FPGA 实验平台与性能测试程序 / SoC 上达到了 107.69 MHz 的主频与 1.02 的 IPC。相比于基线 openLA500 处理器，NOP 核的整体加速比达到了 3.00，IPC 加速比达到了 1.402.

## Usage

Java development kit (JDK) is required to run the project. We use `sbt` as our build tool. To build the project, you need to install `sbt` first. Then, you can run the following command to build the project:

```bash
sbt run
```

Then you can see the build result in `build` directory. Simply copy `mycpu_top.v` to your chiplab FPGA project and you can simulate / run it on FPGA.


## Project Structure

There are several subfolders in the project:

+ `.ci-scripts`: Scripts used in CI.
+ `docs`: Documentation of the project.
+ `src`: Source code of the project.
+ `xilinx_ip`: Xilinx IP cores used in the project.

In `src` folder, there are several subfolders:

+ `blackbox`: Blackbox modules used in the project. (e.g. Multiplier, xpm_memory)
+ `builder`: Taken from VexRiscv project. Contains the definition of the class of Stage, Plugin and Pipeline.
+ `constants`: Contains the definition of constants used in the project, mainly for the LA32R ISA.
+ `debug`: Contains the definition of debug module. (Not used. We use difftest signals instead.)
+ `peripheral`: Contains the definition of peripheral modules. (e.g. AxiBuffer, AxiCrossbar)
+ `pipeline`: Contains the definition of pipeline modules.
+ `utils`: Contains the definition of utility modules.

The `src/pipeline` folder is the heart of our NOP processor's pipeline structure. We dissect the pipeline of NOP CPU into various stages and components, implementing each as Scala modules.

- `core`: This folder houses the central architectural files for the NOP-Core pipeline. Modules here handle key tasks like bypassing network operations (`BypassNetworkPlugin.scala`), managing commit operations and traits (`CommitPlugin.scala`, `CommitTraits.scala`), managing exceptions (`ExceptionMuxPlugin.scala`), and operating on the Reorder Buffer (`ROBFIFOPlugin.scala`, `ReorderBufferModel.scala`), among others.

- `decode`: The decode phase of a CPU pipeline is where instructions fetched from memory are translated into actions the CPU can understand. This directory appears to deal with decoding micro-operations (`DecodeMicroOP.scala`), the actual decoding pipeline (`DecodePipeline.scala`), signals for decoding (`DecodeSignals.scala`), and renaming of registers (`RenameModel.scala`, `RenamePlugin.scala`).

- `exe`: Short for "execute," this directory concerns itself with the CPU's execution stage. Files here manage ALU (Arithmetic Logic Unit) operations (`ALU.scala`), branch unit operations (`BRU.scala`), comparisons (`Comparator.scala`), integer executions (`IntExecutePlugin.scala`), and multiplication and division tasks (`MulDivExecutePlugin.scala`).

- `fetch`: The fetch phase is the first step in a CPU's operation cycle where instructions are retrieved from memory. Modules in this directory cater to functionalities like branch prediction (`BranchPredictModel.scala`), buffering of fetched data (`FetchBufferPlugin.scala`), and instruction caching (`ICachePlugin.scala`).

- `mem`: As the name suggests, this folder focuses on memory operations within the CPU pipeline. This includes address generation (`AddressGenerationPlugin.scala`), managing data cache (`DCachePlugin.scala`), and ensuring the correct retrieval of data and the in-order commit for store / uncached load instructions with modules like `LoadPostProcessPlugin.scala` and `StoreBufferPlugin.scala`.

- `privilege`: This directory seems to center around privileged operations and states of the CPU. It features modules for Control Status Registers (`CSRPlugin.scala`), exception handling (`ExceptionHandlerPlugin.scala`), interrupt handling (`InterruptHandlerPlugin.scala`), and the Memory Management Unit (`MMUPlugin.scala`).


## Supplementary Resources

Also see our:

- SoC Design: [NOP-SoC](https://github.com/NOP-Processor/NOP-SoC)
- PMON Adaptation: [NOP-PMON](https://github.com/NOP-Processor/NOP-PMON)
- Linux Adaptation: [NOP-Linux](https://github.com/NOP-Processor/NOP-Linux)


## Acknowledgement

We are grateful to the following open-source projects:

+ [ZenCove](https://github.com/zencove-thu/zencove-zoom)
+ [VexRiscv](https://github.com/SpinalHDL/VexRiscv)
+ [openLA500](https://gitee.com/loongson-edu/nscscc-openla500)
