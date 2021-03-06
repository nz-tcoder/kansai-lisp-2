## common lispを紹介する<br>プログラムの一例

nz_tcoder

---

## 目次
* はじめに
* 数独リゾルバ
* 比較してみる
* まとめ

---


## はじめに
lispはマイナーです。<!-- .element: style="text-align: left;" -->

* 原因は? もっと広めるには？
 * 噂は聞いていても、実際にプログラムを見る/触ることがない？
 * 記号処理、人工知能などのキーワードが敷居を高くしているかも？
 * リスト処理は何がうれしいか分からない？

他の言語で書かれたプログラムをlispで書くことで、lispの良さを伝えられないか。<!-- .element: style="text-align: left;" -->

---

## 数独リゾルバ

プログラム言語Ruby[1]のイントロダクション<!-- .element: style="text-align: left;" -->

<div style="text-align: left;">
<blockquote><!-- .element: style="margin-left: 20px; text-align: left;font-size:x-large;" -->
コメントと空行を除くと、ちょうど129行のコードが残る。<br>
...(中略)... <br>
このサンプルは、Rubyのパワーと表現力をよく示していると思うがどうだろうか。
</blockquote>
</div>


ではcommon lispで書くとどうなる？<!-- .element: style="text-align: left;" -->

パワーと表現力は？<!-- .element: style="text-align: left;" -->

---

## 数独リゾルバ(概要)
* possible: セル(row,col)について、配置可能な数字を求める。<!-- .element: style="text-align: left; font-size:xx-large;" -->

<img src="img/resolver1.png" style="border:0px"></img>


---


## 数独リゾルバ(概要)
* scan:
 1. 未設定の各セルに対しpossibleを呼ぶ(イテレータ:each_unknown利用)<!-- .element: style="font-size:xx-large;" -->
 1. possibleの結果(配置可能な数字)が一つであれば、その値をセルに設定する。<!-- .element: style="font-size:xx-large;" -->
 1. セルの設定ができなくなったら終了する。終了時には、possibleの結果が最も少ないセルのrow、col、配置可能な数字のリストを返す。<!-- .element: style="font-size:xx-large;" -->

* resolve: <!-- .element: style="font-size:xx-large;" -->
 1. scanを呼び出す。<!-- .element: style="font-size:xx-large;" -->
 1. 数独が解けていなければ、配置可能な数字を順番に試す(再帰的にresolveを呼ぶ) <!-- .element: style="font-size:xx-large;" -->


---


## common lisp版の方針
* オブジェクト指向(CLOS)は使わない。
* common lisp版イテレータの実装はしない。
* 数独の表現には多次元配列(Array)を用いる。
 * (aref *配列* *i* *j* ...) <br>
  多次元でもOK
 * (row-major-aref *配列* *i*) <br>
  *i* は row-major order


--

### row-major order

<div style="text-align: left;">
<img src="img/row-major-1.png" style="border:0px"></img>

<img src="img/row-major-2.png" style="border:0px"></img>
</div>

---

## 比較してみる(0)
#### each_unknown(ruby)
```ruby
BoxOfIndex = [
  0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2,
  3,3,3,4,4,4,5,5,5,3,3,3,4,4,4,5,5,5,3,3,3,4,4,4,5,5,5,
  6,6,6,7,7,7,8,8,8,6,6,6,7,7,7,8,8,8,6,6,6,7,7,7,8,8,8
].freeze

def each_unknown
  0.upto 8 do |row|             # For each row
    0.upto 8 do |col|           # For each column
      index = row*9+col         # Cell index for (row,col)
      next if @grid[index] != 0 # Move on if we know the cell's value 
      box = BoxOfIndex[index]   # Figure out the box for this cell
      yield row, col, box       # Invoke the associated block
     end
  end
end
```

---

## 比較してみる(0)
#### unknown_cells(common lisp)
```lisp
(defconstant +box-index-list+
  (loop for n in '(0 3 6)
     append (loop repeat 3
               append (loop repeat 3 
                         for i from n
                         append (loop repeat 3 collect i)))))

(defun box-index-of (idx)
  (nth idx +box-index-list+))
    
(defun unknown-cells (pz)
  (loop repeat (array-total-size pz)
     for idx from 0
     for cell = (row-major-aref pz idx)
     if (= cell 0) collect (list (floor idx 9) (mod idx 9) (box-index-of idx))))
```

---

## 比較してみる(1)
#### possible(ruby)
```ruby
def possible(row, col, box)
  AllDigits - (rowdigits(row) + coldigits(col) + boxdigits(box))
end

def rowdigits(row)
  @grid[row*9,9] - [0]
end

def coldigits(col)
  result = []                # Start with an empty array
  col.step(80, 9) {|i|       # Loop from col by nines up to 80
    v = @grid[i]             # Get value of cell at that index
    result << v if (v != 0)  # Add it to the array if non-zero
  }
  result                     # Return the array
end

BoxToIndex = [0, 3, 6, 27, 30, 33, 54, 57, 60].freeze

def boxdigits(b)
  i = BoxToIndex[b]
  [
     @grid[i],    @grid[i+1],  @grid[i+2],
     @grid[i+9],  @grid[i+10], @grid[i+11],
     @grid[i+18], @grid[i+19], @grid[i+20]
  ] - [0]
end
```

---

## 比較してみる(1)
#### possible(common lisp)
``` lisp
(defun possible (pz row column box)
  (set-difference +all-digits+
                  (remove-duplicates (append (row-digits pz row)
                                             (column-digits pz column)
                                             (box-digits pz box)))))

(defun row-digits (pz row)
  (remove-zero (mapcar #'(lambda (x) (aref pz row x))
                       '(0 1 2 3 4 5 6 7 8))))

(defun column-digits (pz column)
  (remove-zero (mapcar #'(lambda (x) (aref pz x column))
                       '(0 1 2 3 4 5 6 7 8))))

(defconstant +box-index-list+
  (loop for n in '(0 3 6)
     append (loop repeat 3
               append (loop repeat 3 
                         for i from n
                         append (loop repeat 3 collect i)))))

(defun box-digits (pz box)
  (remove-zero (mapcar #'(lambda (x) (row-major-aref pz x))
                       (loop for digit in +box-index-list+
                          for row-major-index from 0
                          if (= digit box) collect row-major-index))))
```

---

## 比較してみる(2)
#### scan(ruby)
``` ruby
def Sudoku.scan(puzzle)
  unchanged = false  # This is our loop variable

  until unchanged 
    unchanged = true
    rmin,cmin,pmin = nil
    min = 10

    puzzle.each_unknown do |row, col, box|
      p = puzzle.possible(row, col, box)
      
      case p.size
      when 0  
        raise Impossible
      when 1  
        puzzle[row,col] = p[0] 
        unchanged = false
      else
        if unchanged && p.size < min
          min = p.size
          rmin, cmin, pmin = row, col, p
        end
      end
    end
  end
  
  return rmin, cmin, pmin
end
```

---

## 比較してみる(2)
#### scan(common lisp)

``` lisp
(defun scan (pz)
  (loop named outer for cells = (unknown-places pz)
     do
       (loop for (r c b) in cells
          for digit-list = (possible pz r c b)
          if (null digit-list) do               ;NG case
            (signal 'impossible)
          else if (= (length digit-list) 1) do
            (setf (aref pz r c) (car digit-list))
          and count r into update
          else minimize (length digit-list) into min
          and collect (list r c digit-list) into not-determined
          finally
            (if (zerop update)
                (return-from outer
                  (values (find min not-determined
                                :key #'(lambda (x) (length (third x))))
                          (= min 0)))))))
```

---

## 比較してみる(3)
#### solve(ruby)
``` ruby
def Sudoku.solve(puzzle)
  puzzle = puzzle.dup
  r,c,p = scan(puzzle)
  return puzzle if r == nil    

  p.each do |guess|
    puzzle[r,c] = guess
    begin
      return solve(puzzle)
    rescue Impossible
      next
    end
  end

  raise Impossible
end
```

---

## 比較してみる(3)
#### solve(common lisp)
```lisp
(defun solve (puzzle)
  (let ((pz (copy-array puzzle)))
    (multiple-value-bind (possible finish) (scan pz)
      (if finish
          pz
          (destructuring-bind (r c guess-list) possible
            (loop for guess in guess-list
               do
                 (setf (aref pz r c) guess)
                 (handler-case
                     (return (solve pz))
                   (impossible ()))
               finally
                 (signal 'impossible)))))))
```

---

## まとめ
* 90行弱のプログラム
 * 手を抜いた部分もあったかもしれない
 * lispのインデント
* '-'を変数名、関数名に使えるのは楽
* 他にない機能(destructuring-bind,loop,condition(cltl2),multiple-value-bindなど)を紹介できた


--

### scanの末尾を比較

ruby <!-- .element: style="text-align: left; font-size:xx-large;" -->

```
        if unchanged && p.size < min
          min = p.size
          rmin, cmin, pmin = row, col, p
        end
      end
    end
  end

  return rmin, cmin, pmin
end
```

common lisp <!-- .element: style="text-align: left; font-size:xx-large;" -->

```
   (if (zerop update)
       (return-from outer
         (values (find min not-determined
                       :key #'(lambda (x) (length (third x))))
                 (= min 0)))))))
```


--


### possbileでの比較

ruby <!-- .element: style="text-align: left; font-size:xx-large;" -->

```
def possible(row, col, box)
  AllDigits - (rowdigits(row) + coldigits(col) + boxdigits(box))
end
```

common lisp <!-- .element: style="text-align: left; font-size:xx-large;" -->

```
(defun possible (pz row column box)
  (set-difference +all-digits+
                  (remove-duplicates (append (row-digits pz row)
                                             (column-digits pz column)
                                             (box-digits pz box)))))
```

---


## 参考文献
1. プログラミング言語Ruby(David Flanagan,まつもとゆきひろ共著,オライリージャパン,<br>
Example code(http://examples.oreilly.com/9780596516178/RPLExamples.tar.gz))
1. Common Lisp Recipes(Edumnd Weitz著,Apress)
1. 実践Common Lisp(Peter Seibel著,オーム社)<br>(英語版 http://www.gigamonkeys.com/book/)
1. 数独問題集(http://www.sudokugame.org/)

