<center><h2>实验中注意点</h2></center>

###1. 跑实验前确定build_define.py中是否注明需要跑define.
   * 如build_benchmark.sh需要考虑多个benchmark通过\#define BENCHMARK_NAME来确定，但是在build_generate_benchmark.sh中要保证只有一个#define文件，由于其是在脚本中确定FunctionName，如果有多个#define则可能导致一个生成的benchmark重复跑多次。
###2. 再确定build_config.py文件。
   * 如果是cpu程序，需要搞清楚生成Tiling的方法是否是后面新加的，而不是类似于3,1,1,1,11,11,11,2,2,2这类。如果是GPU程序需要保证CudaTilling没有写错。CUDABlocking而不是Tiling_CUDA。还有就是注意生成的新旧，是否是按照公式num_array*4*(loop_radius*2+1)*DX*DY < 49152。
###3. 再确定脚本正确性。
   * 确定FunctionName没有错
   * 确定ProgrammingModel没有错，注意平台，CPU平台是cpu，而GPU平台是cuda
   * 确定生成文件是cpu平台是--ref而gpu平台是--cuda
   * 确定获取时间的脚本正确性，在CPU是用awk程序来获取，在GPU是通过get_time_gflops.py来获取
   * 判定compile.sh正确性，目前没有考虑OMP，未来可能需要加入-fopenmp注意是否都开-O2
   * 类似无法update的时候注意一点是否传入R错误，没有key1=value1 **::** key2=value2的情况

###4. 备份和恢复数据库
   * mysqldump -hhostname -uusername -ppassword databasename | gzip > backupfile.sql.gz
   * gunzip < backupfile.sql.gz | mysql -uusername -ppassword databasename
