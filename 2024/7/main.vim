let g:inp = readfile('input.txt')
let g:total1 = 0
let g:total2 = 0

function! Solve1(re, xs)
    let possible = [a:xs[0]]
    for i in range(1,len(a:xs) - 1)
        let res = map(copy(possible), {_, v -> a:xs[i] + v}) + map(copy(possible), {_, v -> a:xs[i] * v})
        let possible = res
    endfor
    return index(possible, a:re) >= 0 
endfunction

function! Solve2(re, xs)
    echo 'Testing: ' .. a:re
    let possible = [a:xs[0]]
    for i in range(1,len(a:xs) - 1)
        let res = map(copy(possible), {_, v -> a:xs[i] + v}) + map(copy(possible), {_, v -> a:xs[i] * v}) + map(copy(possible), {_, v -> str2nr(string(v) .. string(a:xs[i]))})
        let possible = res
    endfor
    return index(possible, a:re) >= 0 
endfunction

for line in g:inp
    let [sre, sxs] = split(line, ': ')
    let re = str2nr(sre)
    let xs = split(sxs, ' ')->map({_, v -> str2nr(v)})
    let result1 = Solve1(re, xs)
    let g:total1 += result1 * re
    let result2 = Solve2(re, xs)
    let g:total2 += result2 * re
endfor

echo g:total1
echo g:total2
quit!
