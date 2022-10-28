# 编译原理实验指导说明

1. 实验环境为Ubuntu20.04系统，安装有LLVM-10，clang-10，git，vscode-server及相关插件等基础软件。
2. 实验内容的源为<http://222.20.94.23:24080/dog/minic>，可通过git拉取到本地，并把文件夹命名为miniC，即在路径下有/ubuntu/cse/miniC/。不要修改已存在的文件结构，避免判题错误。
3. 在系统中申请虚拟机后，使用vscode连接到虚拟机，对应的账号和密码以课程发布为准，工作目录为家目录下的miniC文件夹。使用vscode客户端，点击左上角的文件菜单，选择打开文件夹，选择miniC后，即可进入工作目录。此时你将获得一个搭配好环境的编程环境。
4. 为了更加良好的体验，可以安装`Lex`,`LLVM`,`Bison`等插件。
5. 请从lab1实验做起，并在实验系统中随时提交作业。
6. 在文件夹test_and_answer中，给出了实验系统用来测试的脚本副本，可以参考或执行其中的内容自行预先测试。执行方式参考`python3 lab_dispatch.py lab1 judge_101.sh`。
7. 在文件夹answers中，给出了每个实验子任务的样例输出的正确结果的副本，可以对比自己测试的输出和对应的正确结果，进行调试。
8. 你拥有对miniC操作的所有权限，但请注意不要破坏文件结构，同时注意随时保存学习内容。可以删除.git文件夹后使用git进行内容管理，也可以直接将相关文件保存到本地。
9. 当出现不可恢复错误时，可删除并重新申请虚拟机，随后恢复上述内容即可。
