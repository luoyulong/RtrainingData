#RtrainingData
## 关于
利用R语言来挖掘特征参数、优化参数与性能之间的关系
- performanceDB.R 包含一些操作数据库和建模的函数
- test 文件夹包含一些R的测试文件
## 代码风格
本项目暂约定一下代码风格(不断逐条添加中)，该规范参考[Google R style guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)
### 关于空格
- 所有代码使用2个空格缩进
- 所有二元操作符两边添加空格(=, +, -, <-, etc)
- "," 和for循环语句中的";" 后面跟上一个空格
- 条件、分支保留字，如 if for while else switch 等后添加一个空格
- 在所有左括号前添加空格，除函数调用

### 关于大括号
- 统一采用紧凑的格式

### 关于赋值
- 对于赋值使用<-而不要=，R库函数几乎都是<-来赋值
- 函数默认值时使用=。

### 关于命名
- 文件名用.R表示,文件名小写开头，接下来的字母大写如performanceDB.R
- 在标识符中不要使用下划线 ( _ ) 或连字符 ( - ). 标识符应根据如下惯例命名。变量名应使用点 (.) 分隔所有的小写字母或单词;函数名首字母大写, 不用点分隔 (所含单词首字母大写); 常数命名规则同函数, 但需使用一个 k 开头。
<pre>
如：
variable.name 
正例: avg.clicks 
反例: avg_Clicks , avgClicks 
FunctionName 
正例: CalculateAvgClicks 
反例: calculate_avg_clicks , calculateAvgClicks 
函数命名应为动词或动词性短语. 
例外: 当创建一个含类 (class) 属性的对象时, 函数名 (也是constructor) 和类名 (class) 应当匹配 (例如, lm). 
kConstantName 
</pre>

### 文件各个部分顺序
- #!/usr/bin/env Rscript
- 作者声明
- 文件描述
- source()和library()函数
- 函数定义
- 可执行代码
- **对于测试代码需要放到一个单独的文件，如originFileName_test.R**
<pre>
#!/usr/bin/env Rscript
#
# Author: XXX(XXX@xx.com)
# Date: 2016-12-3
#
# Desc:
#     This file is for XXX
source("XX.R")
library(RODBC)
</pre>

### 关于注释
- 代码注释在#后需要添加一个空格
- 短的注释需要在代码后面添加2个空格
<pre>
# Create histogram of frequency of campaigns by pct budget spent.
hist(df$pct.spent,
     breaks = "scott",  # method for choosing number of buckets
     main   = "Histogram: fraction budget spent by campaignid",
     xlab   = "Fraction of budget spent",
     ylab   = "Frequency (count of campaignids)")
</pre>
- 函数注释如下
<pre>
CalculateSampleCovariance <- function(x, y, verbose = TRUE) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}
</pre>

###其他
- **一行语句不超过80个字符**
