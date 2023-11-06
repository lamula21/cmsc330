def fib(n)
    first = 0
    second = 1
    fibo_arr = Array.new
    
    if n == 0
        return fibo_arr
    elsif n == 1
        return fibo_arr.append(first)
    else
        while n != 0
            fibo_arr.append(first)
            next_term = first + second
            
            # Update
            first = second
            second = next_term
            n -= 1    
        end

        return fibo_arr
    end

end

def isPalindrome(n)
    string = n.to_s
    len = string.length

    for i in 0..(len/2 - 1) do
        if string[i] != string[len - i - 1]
            return false
        end
    end

    return true
end

def nthmax(n, a)
    a.sort!.reverse!
    return a[n]
end

def freq(s)
    freq = Hash.new(0)

    if s == ""
        return s
    end

    for index in 0...s.length
        # { a: 1, b: 2, c: 3 }
        freq[s[index]] += 1 
    end

    max_value = freq.values.max
    return freq.key(max_value)
end

def zipHash(arr1, arr2)

    newHash = Hash.new(0)

    # if array does not have the same length
    if arr1.length != arr2.length
        return nil
    end

    # Iterate over array and save it as Hash
    for index in 0...arr1.length
        newHash[arr1[index]] = arr2[index]   
    end

    return newHash

    # ANOTHER WAY
    #array = arr1.zip(arr2) # => [['BO','BOLIVIA'],['BR','BRAZIL']]
    #hash = array.to_h # => {'BO' => 'BOLIVIA','BR' => 'BRAZIL'}
    #return hash

end

def hashToArray(hash)
    arr1 = hash.keys
    arr2 = hash.values

    return arr1.zip(arr2)
end

def maxProcChain(init, procs)
    
    result_arr = Array.new(0)
    result_arr.push(init) # x
    # Iterate [ procA, procB, procC]
    while procs.length > 0
        procsCopy = procs.clone
        result = init


        for each in procs do
            result = each.call(result) # ProcC(ProcB(ProcA(x)))
            result_arr.push(result) # ProcA(x) | ProcB(ProcA(x)) | ProcC(ProcB(ProcA(x)))

            result_arr.push(each.call(init))    # Proc#(x)
        end
        
        # 
        while procsCopy.length >= 2
            result = init   # reset variable
            indexDelete = 1

            # [procA, procC]
            procsCopy.delete_at(indexDelete) 

            for each in procsCopy do
                result = each.call(result) # ProcC(ProcA(x))
                result_arr.push(result) # ProcA(x) | # ProcC(ProcA(x))
            end 
        end

        
        # [procB, procC]
        procs.shift
    end

    # Return max value in the array
    return result_arr.max
end
