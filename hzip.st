#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"Arya Kashani"
"akashani"
"1474098"
"CMPS 112"
"Assignment 3"

flagD := false.
flagT := false.
flagC := false.
flagU := false. 

nl := Character nl.

FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

"bits->outfile"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      myStream nextPutByte: currentByte.
      self clearByte.
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"bits<-inputfile"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ true ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count 
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: isLeaf.
   ]

]

Leaf subclass: Tree [
   |left right|
   left [ ^ left ]
   right [ ^ right ]
   isLeaf [ ^ false ]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf := false.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ] 

   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      visitor value: char value: isLeaf.
   ]

]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      |y z|
      y := ''.
      z := ''.
      self isPrint ifTrue: [ ^ ' ', self asString, ' '. ]
                   ifFalse: [
                      y := String new: 3.
                      y at: 1 put: $x.
                      y at: 2 put: $0.
                      z := (((self asInteger printStringRadix: 16) asString) copyFrom: 4 to: (((self asInteger printStringRadix: 16) asString) size)) asString.
                      ^ y , z.
                   ]
   ]
]

"main"

queue := SortedCollection new.

frequencytable := ZeroArray new: 257.
frequencytable at: 256 put: 1.

|writefile readfile outfile argi|
argi := 0.

Smalltalk arguments: '-d -t -c -u'
do: [:flag :arg |
    (flag = $d) ifTrue: [ flagD := true. ].
    (flag = $c) ifTrue: [ flagC := true. ].
    (flag = $u) ifTrue: [ flagU := true. ].
    (flag = $t) ifTrue: [ flagT := true. ].

    (arg ~= nil) ifTrue: [
        argi = 0
            ifTrue: [
                flagD ifTrue:[
                    stdout <<'input filename: ' << arg <<nl.
                ].
                writefile := FileStream open: arg 
                                        mode: FileStream read.
                readfile := FileStream open: arg 
                                        mode: FileStream read.
            ]
            ifFalse: [
                flagD ifTrue: [
                     stdout <<'output filename: ' << arg <<nl.
                ].
                outfile := FileStream open: arg 
                                        mode: FileStream write.
            ].
        argi := argi + 1.
    ].
].

"creating encodeTable"
|tree exitStatus|
exitStatus := 0.

flagU ifFalse: [
    encodeTable := ZeroArray new: 257.

    [writefile atEnd not] whileTrue: [
        |ch|
        ch := writefile next asInteger.
        frequencytable incr: ch.
    ].

    frequencytable keysAndValuesDo: [:k :count |
        (count notNil and: [count > 0]) ifTrue: [
            queue add: (Leaf new: k count: count).
        ]
    ].

    [queue size > 1] whileTrue: [
        |leftChild rightChild|
        leftChild := queue removeFirst.
        rightChild := queue removeFirst.
        newTree := ( Tree new: (leftChild char) 
                        count: (leftChild count) + (rightChild count)
                        left: leftChild right: rightChild).
        queue add: newTree.
    ].

    tree := queue removeFirst.

    tree inorder: [:char :str |
        encodeTable at: char put: str.
    ] prefix: ''.

    binaryTree := ''.
    tree postorder: [:char :isLeaf |
        isLeaf ifFalse: [binaryTree := binaryTree, '1'.]
        ifTrue: [
            bin := ''.
            char = 256
            ifTrue: [bin := '000000001']
            ifFalse: [
                char = 0 ifTrue: [bin := '000000000']
                ifFalse: [
                    8 to: 1 by: -1 do:[:n|
                        bin := bin, ((char bitAt:n) printString).
                    ]
                ].
            ].
            binaryTree := (binaryTree, '0', bin).
        ].
    ].
    flagT ifTrue: [
        encodeTable keysAndValuesDo: [:k :count |
            (count notNil and: [count > 0]) ifTrue: [
                (k = 256) ifTrue: [
                    stdout << 'EOF' << '       '
                       << (frequencytable at: k) <<'  '<< count << nl.
                ]
                ifFalse: [
                    (k = 32) ifTrue: [
                        ((frequencytable at: k) > 99) ifTrue: [
                          stdout << 'x20' << '     '<< 
                            (frequencytable at: k) <<'  '<<count<<nl.
                        ]
                        ifFalse: [
                          ((frequencytable at: k )> 9) ifTrue: [
                            stdout << 'x20' << '      ' <<
                              (frequencytable at: k)<<'  '<<count<<nl.
                          ]
                          ifFalse: [
                            stdout << 'x20' << '       ' << 
                              (frequencytable at: k)<<'  '<<count<<nl.
                          ].
                        ].
                    ]
                    ifFalse: [
                      ((frequencytable at: k) > 99) ifTrue: [
                        stdout <<  k asCharacter visible << '     '<<
                          (frequencytable at: k) <<'  '<< count << nl.
                      ]
                      ifFalse: [
                        ((frequencytable at: k) > 9) ifTrue: [
                          stdout<< k asCharacter visible <<'      '<<
                          (frequencytable at: k)<<'  '<<count<<nl.
                        ]
                        ifFalse: [
                          stdout<< k asCharacter visible<<'       '<<
                          (frequencytable at: k)<<'  '<<count<<nl.
                        ].
                      ].
                    ].
                ].
            ].
        ].
    ].

"compress"
    flagC ifTrue: [
        outfile ~= nil ifTrue: [
            flagD ifTrue: [
                stdout <<nl<< 'flagC = '<<flagC <<'outfile'<< nl.
            ].
            oBytes := OutBits new: outfile.
            1 to: (binaryTree size) by: 1 do: [:index |
                bit := ((binaryTree at: index) asInteger) - 48.
                oBytes writeBit: bit.
            ].
            oBytes writeBit: 1.
            [readfile atEnd not] whileTrue: [
                |orderCh|
                orderCh := readfile next asInteger.
                code := (encodeTable at: orderCh).
                1 to: (code size) by: 1 do: [:index |
                    bit := ((code at: index) asInteger) - 48.
                    oBytes writeBit: bit.
                ].
            ].
            EOF := (encodeTable at: 256).
            1 to: (EOF size) by: 1 do: [:index |
                bit := ((EOF at: index) asInteger) - 48.
                oBytes writeBit: bit.
            ].
            oBytes flushByte.
        ]
        ifFalse: [
            flagD ifTrue: [
                stdout <<nl<< 'flagC = '<< flagC <<'no outfile'<< nl.
            ].
            stdout <<nl<< 'Encoded input to binary' << nl.
            stdout <<binaryTree<<1.
            [readfile atEnd not] whileTrue: [
                |orderCh|
                orderCh := readfile next asInteger.
                code := (encodeTable at: orderCh).
                stdout << code.
            ].
            EOF := (encodeTable at: 256).
            stdout << EOF.
        ].
    ].
].

"decompression"
(flagU) ifTrue:[
   |x oBytes stack|
    x := false.
   flagD ifTrue: [
      stdout << 'decompression' << nl. 
   ].
   oBytes := OutBits new: outfile.  
   stack := OrderedCollection new.
   fileBits := BitStream new: writefile.
   treeDone := false.
   treeFinished := ''.
   node := ''.   
   
   "bit<-input"
   [(fileBits atEnd not) & (x not)] whileTrue: [
     treeNode := fileBits nextBit.
     leafCH := 0.
     treeDone ifFalse:[
        flagD ifTrue: [
            stdout << 'stack' << stack << nl. 
        ].
        "input->leaf"
        treeNode = 0 ifTrue:[
           8 to: 1 by: -1 do: [:n |
              bit := fileBits nextBit.
              leafCH := leafCH bitAt: n put: bit.
           ].
           leafCH = 0 ifTrue:[
              specialbit := fileBits nextBit.
              specialbit = 1 ifTrue:[
                  leafCH := leafCH bitAt: 9 put: 1.
              ].
           ].
           stack add: (Leaf new: leafCH count: '').
        ].

        "leaves->tree" 
        treeNode = 1 ifTrue:[
           stack size > 1 ifTrue:[
              rightChild := stack removeLast.
              leftChild := stack removeLast.
              newtree := (Tree new: -1 count: '' left: leftChild right: rightChild).
              stack add: newtree.
           ] 
           ifFalse:[
             treeDone := true.
             treeFinished := stack removeLast.
             node := treeFinished.
           ].
        ].
     ]
     "if treeDone, decodes data"
     ifTrue:[
        flagD ifTrue: [
            stdout << 'stack' << stack << nl. 
        ].
       node isLeaf ifTrue:[
                ((node char)=256) ifTrue:[
                    x := true.
                ] 
                ifFalse:[
                    stdout nextPut: (Character value: (node char)).
                    node := treeFinished.
                ].
        ]
        ifFalse:[
          treeNode = 0 ifTrue:[
              node := node left.
              node isLeaf ifTrue:[
                  x not ifTrue:[  
                      ((node char)=256) ifTrue:[
                          x := true.
                      ]
                      ifFalse:[
                          outfile = nil ifTrue:[
                              stdout nextPut: (Character value: (node char)).
                          ]
                          ifFalse:[
                              8 to: 1 by: -1 do: [:index |
                                  bit := (node char) bitAt: index.
                                  oBytes writeBit: bit.
                              ].
                          ].
                          node := treeFinished.
                      ].
                  ].
              ]
            ]
            ifFalse:[
              node := node right.
              node isLeaf ifTrue:[
                  x not ifTrue:[
                      ((node char)=256) ifTrue:[
                          x := true.
                      ] 
                      ifFalse:[
                          outfile = nil ifTrue:[
                                stdout nextPut: (Character value: (node char)).
                          ]
                          ifFalse:[
                              8 to: 1 by: -1 do: [:index |
                                  bit := (node char) bitAt: index.
                                  oBytes writeBit: bit.
                              ].
                          ].
                          node:=treeFinished.
                      ].
                  ].
              ]
          ].
        ].
    ].
  ].
]. 
flagD ifTrue: [
    stdout << nl << 'Exit Status: ' << exitStatus << nl.
].
writefile close.
outfile ~= nil ifTrue:[
    outfile close.
].