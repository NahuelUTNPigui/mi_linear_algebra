import sequtils
import math
type Matriz* = ref object
    valores* :  seq[seq[float]]

type Vector* = ref object
    valores* :  seq[float]


type VectorMatriz = object
    matriz:Matriz
    vector:Vector

type SliceMatriz* = object
    matriz:Matriz
    #valores*: seq[seq[float]]
    start_fila*:int
    start_columna*:int
    end_fila*:int
    end_columna*:int


type SliceVector* = object
    vector : Vector
    #valores* :seq[float]
    start*:int
    final*:int

type SliceVectorFromMatriz = object
    matriz : Matriz
    #valores:seq[seq[float]]
    start_fila*:int
    start_columna*:int
    end_fila*:int
    end_columna*:int




#Me suena que no hace falta
type Numero* = object
    valor*:float


type MATRIX_TYPES* = enum
    SINGULAR,INCONSISTENT,IRREDUCIBLE,UNFACTORIZABLE

type ASSERTION* = enum
    NONE,WAR,DEFAULT,THROW
type MULT_MAT* = enum
    AUTO,STRASSEN,NAIVE
# Overload operators
proc `$`*(m:Matriz):string=
    var s = "valores:  \n"
    for fila in m.valores:
        s &= "\t"
        for n in fila:
            s &= $n & " "
        s &= "\n"
    s
    
    

proc `$`*(v:Vector):string=
    var s = "valores: \n"
    s &= "\t"
    for n in v.valores:
        s &=  $n & " "
    s
    
    

proc `[]`*(svm:SliceVectorFromMatriz,i:int):float=
    # Si es vector fila
    if svm.start_fila == svm.end_fila:
        return svm.matriz.valores[svm.start_fila][svm.start_columna+i]
    
    #VEctor columna
    else:
        return svm.matriz.valores[svm.start_fila+i][svm.start_columna]

proc `[]`*(sm:SliceMatriz,i:int):seq[float]=
    return sm.matriz.valores[i]

proc `[]=`*(sm:var SliceMatriz,i:int,fila:seq[float])=
    sm.matriz.valores[i] = fila

proc `[]`*(sm:SliceMatriz,i,j:int):float=
    return sm.matriz.valores[sm.start_fila+i][sm.start_columna+j]

proc `[]=`*(sm:var SliceMatriz,i,j:int,valor : float)=
    sm.matriz.valores[sm.start_fila+i][sm.start_columna+j] = valor  

proc `[]`*(sv:SliceVector,i:int):float=
    return sv.vector.valores[sv.start+i]

proc `[]=`*(sv: var SliceVector,i: int, valor:float)=
    sv.vector.valores[sv.start+i] = valor





# Constructores
proc newMatriz*(valores:seq[seq[float]]):Matriz=
    Matriz(valores:valores)

proc newMatriz*(n,m:int):Matriz=
    var valores:seq[seq[float]] = newSeq[seq[float]](n)
    for i in countup(0,n-1):
        valores[i] = newSeq[float](m)
    
    newMatriz(valores)

    
proc newVector*(valores:seq[float]):Vector=
    Vector(valores:valores)

proc newVector*(n:int):Vector=
    var valores = newSeq[float](n)
    newVector(valores)

proc newVectorMatriz(matriz:Matriz,vector:Vector):VectorMatriz=
    VectorMatriz(matriz:matriz,vector:vector)
#Me suena que no hace falta

proc newNumero*(valor:float):Numero=
    Numero(valor:valor)

proc newSliceMatriz*(m:Matriz,start_fila,start_columna,end_fila,end_columna:int):SliceMatriz=
    SliceMatriz(matriz:m,start_fila:start_fila,start_columna:start_columna,end_fila:end_fila,end_columna:end_columna)

proc newSliceMatriz*(sm:SliceMatriz,start_fila,start_columna,end_fila,end_columna:int):SliceMatriz=
    SliceMatriz(
        matriz:sm.matriz,
        start_fila:sm.start_fila+start_fila,
        start_columna:sm.start_columna+start_columna,
        end_fila:sm.start_fila+end_fila,
        end_columna:sm.start_columna+end_columna
    )

proc newSliceVector*(v:Vector,start,final:int):SliceVector=
    SliceVector(vector:v,start:start,final:final)

proc newSliceVector*(sv:SliceVector,start,final:int):SliceVector=
    SliceVector(vector:sv.vector,start:sv.start+start,final:sv.final+final)

proc newSliceVectorFromMatriz*(m:Matriz,start_fila,start_columna,end_fila,end_columna:int):SliceVectorFromMatriz=
    SliceVectorFromMatriz(matriz:m,start_fila:start_fila,start_columna:start_columna,end_fila:end_fila,end_columna:end_columna)

# Overload operator
proc `-`*(m1,m2:Matriz):Matriz=
    var valores:seq[seq[float]] = @[]
    for i in countup(0,m1.valores.len-1):
        var fila:seq[float] = @[]
        for j in countup(0,m1.valores[0].len-1):
            fila.add(m1.valores[i][j]-m2.valores[i][j])
        valores.add(fila)
    newMatriz(valores)

proc `+`*(m1,m2:Matriz):Matriz=
    var valores:seq[seq[float]] = @[]
    for i in countup(0,m1.valores.len-1):
        var fila:seq[float] = @[]
        for j in countup(0,m1.valores[0].len-1):
            fila.add(m1.valores[i][j]+m2.valores[i][j])
        valores.add(fila)
    newMatriz(valores)

proc `+=`*(m1:var Matriz,m2:Matriz)=
    for i in countup(0,m1.valores.len-1):
        
        for j in countup(0,m1.valores[0].len-1):
            m1.valores[i][j] += m2.valores[i][j]
        
proc `+=`*(v1:var Vector,v2:Vector)=
    for i in countup(0,v1.valores.len-1):
        v1.valores[i] += v2.valores[i]

proc `-`*(v1,v2:Vector):Vector=
    var valores:seq[float] = @[]
    for i in countup(0,v1.valores.len-1):
        valores.add(v1.valores[i]-v2.valores[i])
    newVector(valores)


proc escale*(sl: var SliceVectorFromMatriz,alfa:float)=
    for i in countup(sl.start_fila,sl.end_fila):
        for j in countup(sl.start_columna,sl.end_columna):
            sl.matriz.valores[i][j] *= alfa

proc escalar*(m: var Matriz, esc:float)=
    for i in countup(0,m.valores.len - 1):
        for j in countup(0,m.valores[i].len - 1):
            m.valores[i][j] *= esc

proc escalar*(v: var Vector, esc:float)=
    for i in countup(0,v.valores.len - 1):
        v.valores[i] *= esc


proc escalarNewVector*(v:Vector,esc:float):Vector=
    var valores:seq[float] = @[]
    for vv in v.valores:
        valores.add(esc*vv)
    newVector(valores)

proc escalarNewMatriz*(m:Matriz,esc:float):Matriz=
    var valores:seq[seq[float]] = @[]
    for fila in m.valores:
        var nueva_fila:seq[float] = @[]
        for vv in fila:
            nueva_fila.add(vv*esc)
        valores.add(nueva_fila)
    newMatriz(valores)



proc maxi*(sl: SliceVectorFromMatriz):int=
    var idx=0
    var max_idx=0
    var max_valor=abs(sl.matriz.valores[sl.start_fila][sl.start_columna])
    for i in countup(sl.start_fila,sl.end_fila):
        for j in countup(sl.start_columna,sl.end_columna):
            let valor = abs(sl.matriz.valores[i][j])
            if valor > max_valor:
                max_valor = valor
                max_idx = idx
            idx += 1
    max_idx

proc swap*(aii:var SliceMatriz,idx:int)=
    if idx != 0:
        var tmp = aii[0]
        aii[0] = aii[idx]
        aii[idx] = tmp

proc swap*(m:var  Matriz,fila:int)=
    if fila != 0 :
        
        var tmp = m.valores[0]
        m.valores[0] = m.valores[fila]
        m.valores[fila] = tmp

proc swap*(m:var  Matriz,filaInicia,filaFinal:int)=
    if filaInicia != filaFinal :
        
        var tmp = m.valores[filaInicia]
        m.valores[filaInicia] = m.valores[filaFinal]
        m.valores[filaFinal] = tmp



proc swapPivot*(v:var Vector,pivot:seq[int])=
    for i in countup(0,pivot.len-1):
        let idx = pivot[i]
        if idx != 0:
            let tmp=v.valores[i]
            v.valores[i] =  v.valores[i + idx]
            v.valores[i + idx] = tmp

proc swapPivot*(m:var Matriz,pivot:seq[int])=
    for i in countup(0,pivot.len-1):
        let idx = pivot[i]
        if idx != 0:
            let tmp=m.valores[i]
            m.valores[i] =  m.valores[i + idx]
            m.valores[i + idx] = tmp


proc identity*(m:Matriz):Matriz=
    var valores:seq[seq[float]] = @[]
    for i in countup(0,m.valores.len-1):
        var fila:seq[float] = @[]
        for j in countup(0,m.valores[0].len-1):
            if i == j :
                fila.add(1.0)
            else:
                fila.add(0.0)
        valores.add(fila)
    newMatriz(valores)

proc identity*(filas:int):Matriz=
    var valores:seq[seq[float]] = @[]
    for i in countup(0,filas-1):
        var fila:seq[float] = @[]
        for j in countup(0,filas-1):
            if i == j :
                fila.add(1.0)
            else:
                fila.add(0.0)
        valores.add(fila)
    newMatriz(valores)

proc norm*(v:Vector):float=
    if v.valores.len == 0:
        return 0.0
    var maxi = abs(v.valores[0])
    for n in v.valores:
        if abs(n)>maxi:
            maxi = abs(n)
    var suma = 0.0
    for n in v.valores:
        suma += pow(n/maxi,2)
    
    return maxi * sqrt(suma)

proc isSquared*(m:Matriz):bool=
    m.valores.len == m.valores[0].len

proc approx*(m1,m2:Matriz,ep = 0.0000001):bool=
    if m1.valores.len != m2.valores.len:
        return false
    if m1.valores[0].len != m2.valores[0].len:
        return false
    for i in countup(0,m1.valores.len-1):
        for j in countup(0,m1.valores[0].len-1):
            if abs(m1.valores[i][j] - m2.valores[i][j])>ep:
                return false
    return true
proc approx*(v1,v2:Vector,ep = 0.0000001):bool=
    if v1.valores.len != v2.valores.len:
        return false
    for i in countup(0,v1.valores.len-1):
        if abs(v1.valores[i] - v2.valores[i])>ep:
            return false
    true

# Si es una matriz de permutacion
proc isPerm*(m:Matriz):bool=
    let ep = 0.0000000001
    for row in m.valores:
        let ceros = filter(row,proc(x:float):bool= abs(x) < ep)
        let unos = filter(row,proc(x:float):bool= abs(x-1) < ep)
        if unos.len != 1:
            return false
        if ceros.len != row.len - 1:
            return false
    return true

proc transpose*(m:Matriz):Matriz=
    
    var valores:seq[seq[float]] = @[]
    
    for j in countup(0,m.valores[0].len-1):
        var fila:seq[float] = @[]
        for i in countup(0,m.valores.len-1):
            let valor=m.valores[i][j]
            fila.add(valor)
        valores.add(fila)

    newMatriz(valores)

proc matmul(m1,m2:Matriz,mm=MULT_MAT.AUTO):Matriz=
    var valores:seq[seq[float]] = @[]
    if mm == NAIVE:
        var valores:seq[seq[float]] = @[]
        for i in countup(0,m1.valores.len-1):
            var fila:seq[float] = @[]
            for j in countup(0,m2.valores[0].len-1):
                var suma = 0.0
                for k in countup(0,m1.valores[0].len-1):
                    suma += m1.valores[i][k] * m2.valores[k][j]
                fila.add(suma)
            valores.add(fila)
        return newMatriz(valores)
        
    elif mm == STRASSEN:
        return newMatriz(@[])
    else:
        if m1.valores.len > 100000000:
            return matmul(m1,m2,STRASSEN)
        else:
            return matmul(m1,m2,NAIVE)
    newMatriz(valores )
proc matmul(v:Vector,m:Matriz):Vector=
    Vector()
proc matmul(m:Matriz,v:Vector):Vector=
    Vector()
proc matmulnaive*(v1,v2:Vector):Matriz=
    var valores:seq[seq[float]] = @[]
    for i in countup(0,v1.valores.len-1):
        var fila:seq[float] = @[]
        for j in countup(0,v2.valores.len-1):
            fila.add(v1.valores[i]*v2.valores[j])
        valores.add(fila)
    newMatriz(valores)

proc matmulnaive*(m1,m2:Matriz):Matriz=
    if m1.valores[0].len != m2.valores.len:
        return Matriz()
    var valores:seq[seq[float]] = @[]
    for i in countup(0,m1.valores.len-1):
        var fila:seq[float] = @[]
        for j in countup(0,m2.valores[0].len-1):
            var suma = 0.0
            for k in countup(0,m1.valores[0].len-1):
                suma += m1.valores[i][k] * m2.valores[k][j]
            fila.add(suma)
        valores.add(fila)
    return newMatriz(valores)

proc matmulnaive*(m:Matriz,v:Vector):Vector=
    if m.valores.len != v.valores.len:
        echo "NO son del mismo tamaño"
        return Vector()
    var valores:seq[float] = @[]
    for i in countup(0,m.valores.len-1):
        var suma = 0.0
        for j in countup(0,v.valores.len-1):
            suma += m.valores[i][j] * v.valores[j]
        valores.add(suma)
    newVector(valores)

proc matmulnaive*(v:Vector,m:Matriz):Vector=
    if m.valores[0].len != v.valores.len:
        echo "No son del mismo tamaño"
        return Vector()
    var valores:seq[float] = @[]
    for i in countup(0,m.valores[0].len-1):
        var suma = 0.0
        for j in countup(0,m.valores.len-1):
            suma += v.valores[j] * m.valores[i][j]
        valores.add(suma)
    newVector(valores)

proc isHermitian*(m:Matriz):bool=
    m.approx(m.transpose)

proc isUnitary*(v:Vector):bool=
    let norma = v.norm()
    if norma > 0.9999 and norma < 1.0001:
        return true
    return false

proc lualg*(m: var Matriz)=
    let rows=m.valores.len
    let cols = m.valores[0].len
    
    for i in countup(0,rows-2):
        
        let alfa = m.valores[i][i]
        let slice_a12=m.newSliceVectorFromMatriz(i,i+1,i,cols-1)
        var slice_a21=m.newSliceVectorFromMatriz(i+1,i,rows-1,i)
        slice_a21.escale(1/alfa)
        
        var slice_A22= m.newSliceMatriz(i+1,i+1,rows-1,cols-1)
        let limite_fila=rows-i-2
        let limite_columna=cols-i-2
        for ki in countup(0,limite_fila):
            for kj in countup(0,limite_columna):
                
                # ESTO ESTA MAL
                # let beta = - (slice_a12[ki] * slice_a21[kj])
                # Esta bien
                let beta = - (slice_a12[kj] * slice_a21[ki])
                
                let a= slice_A22[ki,kj]
                slice_A22[ki,kj] = a + beta

proc lupalg*(m: var Matriz):seq[int]=
    let rows=m.valores.len
    let cols = m.valores[0].len
    var pivot_vector:seq[int] = @[]
    for i in countup(0,rows-2):
        # Lo del pivote
        #var aii = newSliceMatriz(m,i,i,rows-1,cols-1)
        var coli=newSliceVectorFromMatriz(m,i,i,rows-1,i)
        let idx=maxi(coli)
        swap(m,i,idx+i)
        #swap(aii,idx)
        pivot_vector.add(idx)
        # Lu comun
        let alfa = m.valores[i][i]
        let slice_a12=m.newSliceVectorFromMatriz(i,i+1,i,cols-1)
        var slice_a21=m.newSliceVectorFromMatriz(i+1,i,rows-1,i)
        slice_a21.escale(1/alfa)
        
        var slice_A22= m.newSliceMatriz(i+1,i+1,rows-1,cols-1)
        let limite_fila=rows-i-2
        let limite_columna=cols-i-2
        for ki in countup(0,limite_fila):
            for kj in countup(0,limite_columna):
                
                # ESTO ESTA MAL
                # let beta = - (slice_a12[ki] * slice_a21[kj])
                # Esta bien
                let beta = - (slice_a12[kj] * slice_a21[ki])
                
                let a= slice_A22[ki,kj]
                slice_A22[ki,kj] = a + beta
    pivot_vector    


proc separarLU*(m: Matriz):seq[Matriz]=
    var matricesLU=newSeq[Matriz](2)
    var valoresl:seq[seq[float]] = @[]
    var valoresu:seq[seq[float]] = @[]
    for i in countup(0,m.valores.len-1):
        var filau=newSeq[float]()
        var filal=newSeq[float]()
        for j in countup(0,m.valores[0].len - 1):
            if i == j:
                filau.add(m.valores[i][j])
                filal.add(1.0)
            elif i > j:
                filau.add(0.0)
                filal.add(m.valores[i][j])
            else:
                filau.add(m.valores[i][j])
                filal.add(0.0)
        valoresl.add(filal)
        valoresu.add(filau)
    matricesLU[0]=newMatriz(valoresl)
    matricesLU[1]=newMatriz(valoresu)
    return matricesLU
            



proc lsolve*(m:Matriz,v: var Vector)=
    let rows = m.valores.len
    for i in countup(0,rows-2):
        let y1 = v.valores[i]
        var l21 = newSliceVectorFromMatriz(m,i+1,i,rows-1,i)
        var y2 = newSliceVector(v,i+1,rows)
        let limite_fila = rows-i-2
        for ki in countup(0,limite_fila):
            y2[ki] = y2[ki] - y1 * l21[ki]

proc usolve*(m:Matriz,v: var Vector)=
    let rows = m.valores.len
    for i in countdown(rows-1,0):

        let z1 = v.valores[i]
        let v11 = m.valores[i][i]
        var z0 = newSliceVector(v,0,i-1)
        let u01= newSliceVectorFromMatriz(m,0,i,i-1,i)
        
        v.valores[i] = z1/v11
        

        for ki in countup(0,i-1):
            z0[ki] = z0[ki] - (z1/v11) * u01[ki]
        

proc lusolvels*(m:var Matriz,v: var Vector)=
    lualg(m)
    lsolve(m,v)
    usolve(m,v)

#Overwrite el resultado en v
proc solvels*(m:var Matriz,v: var Vector)=
    let pivot = lupalg(m)
    #echo pivot
    v.swapPivot(pivot)
    lsolve(m,v)
    
    usolve(m,v)
   
#[
    Algoritmos auxiliares de separacion, pegado, stackeo
    capaz haya que pensarlo 2 veces
]#
