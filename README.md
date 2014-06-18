#RtrainingData
## 关于
利用R语言来挖掘特征参数、优化参数与性能之间的关系
performanceDB.R 包含一些操作数据库和建模的函数
## 代码风格
本项目暂约定一下代码风格(不断逐条添加中)，该规范参考[Google R style guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml)
###1. 关于空格
- 所有代码使用4个空格缩进
- 所有二元操作符两边添加空格(=, +, -, <-, etc)，除函数定义时的可选参数赋值中的"="
- "," 和for循环语句中的";" 后面跟上一个空格
- 条件、分支保留字，如 if for while else switch 等后添加一个空格
- 在所有左括号前添加空格，除函数调用

###2. 关于大括号
- 统一采用紧凑的格式

###3. 关于命名
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
