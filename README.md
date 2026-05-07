# water_probability

## Chạy code

1. Mở code trong Visual Studio Code.
   
2. Mở Terminal.
   
3. Gõ R (Nhớ tải R trước) hoặc & "C:\Program Files\R\R-4.5.3\bin\R.exe".
   
4. Gõ source("~/Code.R"). Ví dụ: source("C:/Users/Nhu/Desktop/XSTK/water_probability/Code.R").
   
Note: 
- Trước khi chạy, nhớ update read.csv("~/water_potability.csv"). Ví dụ: read.csv("C:/Users/Nhu/Desktop/XSTK/water_probability/water_potability.csv").

- Các output hình ảnh trong folder picture.

- Thêm file output.txt tổng hợp các output.

## Update

1. Thêm print() để in ra hết khi chạy source code.

2. Handling missing values: Không tính toán % missing chỉ có sum.

3. q1: điểm mà 25% dữ liệu nằm bên trái; q3: điểm mà 75% dữ liệu nằm bên trái.

4. iqr = q3 - q1: khoảng giá trị chứa 50% dữ liệu trung tâm.
