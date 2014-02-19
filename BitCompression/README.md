The above codes implement Bit compression using Bit syntax in erlang.


Given records of the following type:
<Name>:<course num1>,<marks1>:<course num2>,<marks2>:<course num3>,<marks3>
Example:
Rahul:cs100,90:cs110,92:cs120,95

Following is the compression method
1 byte: <num courses>
1 byte: <name len>
<name>
1 byte: <course1 len>
<course1 name>
1 byte: <course2 len>
<course2 name>
<other course information>
7 bits: <marks1>
7 bits: <marks2>
<other course marks>
