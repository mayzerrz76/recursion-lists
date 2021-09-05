-------------------------------------------------------------
--------------------Programming Task 3-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------
import Data.Char

-------------------------------------------------------------
-- PROBLEM #1
-- Purpose:
--    allBits str
--        Takes a string input and examines every character to determine
--        if the string is all '1' and '0' chars.  The function returns 
--        False if any chars in str are not '0' or '1'

--Definition:
allBits :: String -> Bool
allBits [] = True
allBits (a:rest)
    | a == '0' || a =='1' = allBits rest
    | otherwise           = False

-- Tests:
--   Tests include the empty string, a string of all '0's, an string of all '1's and
--   a string of all '0's and '1's,  each of which should return True. Tests also include
--   various strings that includes almost all '0's and '1's but not all, or strings that
--   contain no '0's or '1's at all... these cases should return False.

t1a = allBits ""                 -- Should return True (Vacuously True, there are no chars in empty string)
t1b = allBits "00011101011"      -- Should return True (String with all '0's and '1's)
t1c = allBits "000000"           -- Should return True (Sring with all '0's)
t1d = allBits "111111"           -- Should return True (Sring with all '1's)
t1e = allBits "0001011101e00"    -- Should return False (Almost all '0's and '1's)
t1f = allBits "aa##"             -- Should return False (Contains no '0's or '1's)
t1g = allBits "30000111100"      -- Should return False (Leading digit is not a 0 or 1)

-------------------------------------------------------------
-- PROBLEM #2
-- Purpose:
--    triplicate str
--       This function takes a string, and produces a new string in such a way
--       that each character in the original string now occurs 3 times in a row
--       in the returned string. Each char now has 3 total copies so to speak.

-- Definition:
triplicate :: String -> String
triplicate [] = []
triplicate (c:rest) = c:c:c: triplicate rest

-- Tests:
--   Tests include the empty string, a string of 1 char, a string of digits, a string of bits (ie. 0s or 1s)
--   and a string with alternate chars (like punctuation chars).

t2a = triplicate ""                 -- Should return "" (Empty string, no chars to copy)
t2b = triplicate "r"                -- Should return "rrr" (String with only 1 char)
t2c = triplicate "01"               -- Should return "000111" (Sring of two different bits)
t2d = triplicate "1234"             -- Should return "111222333444" (Sring of sequential digits)
t2e = triplicate "!a,"              -- Should return "!!!aaa,,," (String with punctution chars)

-------------------------------------------------------------
-- PROBLEM #3
-- Purpose:
--    voteResult b1 b2 b3
--        This function takes 3 chars, with the assumption that the chars passed
--        in will be '0' or '1'.  The function will return a tuple, the first element
--        of the returned tuple is a char indicating the majority of the bits passed in.
--        The second element of the returned tuple will indicate if all 3 were bits passed 
--        in were the same. (ie. 111 or 000)


-- Definition:
voteResult :: Char -> Char -> Char -> (Char,Bool)
voteResult b1 b2 b3
    | b1 == b2 && b2 == b3 = (b1, True)
    | b1 == b2 || b1 == b3 = (b1, False)
    | otherwise            = (b2, False)

-- Tests:
--   Tests include 3 '0's passed in, 3 '1's passed in. A test with majority '1's, a
--   test with majority '0's

t3a = voteResult '0' '0' '0'        -- Should return ('0', True) (All '0's, unanimous so true in tuple)
t3b = voteResult '1' '1' '1'        -- Should return ('1', True) (All '1's, unanimous so true in tuple)
t3c = voteResult '1' '0' '0'        -- Should return ('0', False) (Majority '0', non unanimous so false in tuple)
t3d = voteResult '0' '1' '1'        -- Should return ('1', False) (Majority '1', non unanimous so false in tuple)


-------------------------------------------------------------
-- PROBLEM #4
-- Purpose:
--    decode lst
--        This function takes a string passed in of '0' and '1' chars that 
--        represent bits and examines the string 3 characters at a time.
--        If the length of the string passed in is not divisible by 3 (ie. missing bits)
--        the function will fill in the missing bits with '0' bits. The function will
--        return a list containing a tuple for every 3 bits in the string passed in.
--        The function voteResult defined in problem 3 explain how the tuples of each 3
--        bits is determined.

-- Definition:
decode :: String -> [(Char,Bool)]
decode [] = []
decode (a:b:c:rest) = (voteResult a b c) : decode rest
decode (a:b:[]) = [(fst(voteResult a b '0'),False)]
decode lst = [('0',False)]

-- Tests:
--   Tests include 2 different tests with no corruption within the string of bits.  Tests also include
--   strings of bits with 1 implicit trailing '0' and 2 implicit trailing '0'. Lastly tests include strings
--   with length that are divisible by 3 but there is corruption within a 3 bit sequence (eg. two '0's and one '1')

t4a = decode "000"           -- Should return [('0', True)] (No corruption, one 3-bit sequence)
t4b = decode "000000111"     -- Should return [('0', True),('0',True),('1',True)] (No corruption all true, multiple 3 bit sequence)
t4c = decode "0001"          -- Should return [('0', True),('0',False)] (2 implicit '0's at end of input string, corruption)
t4d = decode "110111"        -- Should return [('1', False),('1',True)] (Majority '1' within a 3 bit sequence, corruption)
t4e = decode "111001"        -- Should return [('1', True),('0',False)] (Majority '0' within a 3 bit sequence, corruption)
t4f = decode "11100011"      -- Should return [('1', True),('0',True),('1',False)] (1 implicit '0' at end of input string, corruption)

-------------------------------------------------------------
-- PROBLEM #5
-- Purpose:
--     errorFree str
--        Determines if str is error free, by checking if it contains any non '0' or '1' chars
--        as well as checking for data corruption.  (Corruption occurs with missing bits as well
--        as bits that were not triplicated properly eg. 0 -> 001).  The function returns false
--        if either corruption or non '0' or '1' chars occur in the string passed in.

-- Definition:

errorFree :: String -> (String,Bool)
errorFree [] = ("",True)
errorFree str
    | not (allBits str) = ("",False)
    | length err > 0    = (output,False)
    | otherwise         = (output, True)
    where 
      (output,boolLst) = (unzip(decode str))
      err = filter (\x -> not x) (boolLst)

-- Tests:
--   Tests include 2 different tests with no corruption within the string of bits.  Tests also include
--   strings of bits with 1 implicit trailing '0' and 2 implicit trailing '0'. Lastly tests include strings
--   with length that are divisible by 3 but there is corruption within a 3 bit sequence (eg. two '0's and one '1')

t5a = errorFree "000"           -- Should return ("0",True) (No corruption, one 3-bit sequence w/ all '0's)
t5b = errorFree "111111"        -- Should return ("11",True) (No corruption, multiple 3 bit sequence w/ all '1's)
t5c = errorFree "000000111"     -- Should return ("001",True) (No corruption, multiple 3 bit sequence w/ '0's and '1's)
t5d = errorFree "0001"          -- Should return ("00",False) (2 implicit '0's at end of input string, corruption)
t5e = errorFree "110111"        -- Should return ("11",False) (Majority '1' within a 3 bit sequence, corruption)
t5f = errorFree "111001"        -- Should return ("10",False) (Majority '0' within a 3 bit sequence, corruption)
t5g = errorFree "11100011"      -- Should return ("101",False) (1 implicit '0' at end of input string, corruption)

-------------------------------------------------------------
-- PROBLEM #6
-- Purpose:
--    toDigitsRev n
--       This function takes an integer and seperates each digit of the integer,
--       into a list, in such a way that the digits are in reverse order of the original
--       integer (ie. 145 -> [5,4,1])

-- Definition:
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (mod n 10) : toDigitsRev(div n 10)

-- Tests:
--   Tests include a negative number and 0, both which return the empty list. Tests also include larger
--   integers with both an even or odd amount of digits.

t6a = toDigitsRev (-30)      -- Should return [] (No corruption, one 3-bit sequence)
t6b = toDigitsRev 0          -- Should return [] (No corruption all true, multiple 3 bit sequence)
t6c = toDigitsRev 24600      -- Should return [0,0,6,4,2] (2 implicit '0's at end of input string, corruption)
t6d = toDigitsRev 3845       -- Should return [5,4,8,3] (Majority '1' within a 3 bit sequence, corruption)
t6e = toDigitsRev 21         -- Should return [1,2] (Majority '0' within a 3 bit sequence, corruption)
t6f = toDigitsRev 123        -- Should return [3,2,1] (1 implicit '0' at end of input string, corruption)

-------------------------------------------------------------
-- PROBLEM #7
-- Purpose:
--    doubleEveryOther lst
--       This function takes a list of integers and doubles the value of every other
--       integer, starting with the second integer in the list.  The function returns
--       the new list.

-- Definition:
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:ls) = x:(y*2) : doubleEveryOther ls
doubleEveryOther (x:ls) = [x]

-- Tests:
--   Tests include an empty list, a list with length of 1,  a couple lists with odd lengths, and 
--   a couple lists with even lengths.  Some tests include negative numbers for good measure.

t7a = doubleEveryOther []              -- Should return [] (Empty list, returns empty list)
t7b = doubleEveryOther [7]             -- Should return [7] (List with one element, nothing to double)
t7c = doubleEveryOther [3,6,8,20]      -- Should return [3,12,8,40] (Even length list, 2nd and 4th element doubled)
t7d = doubleEveryOther [4,7,9,-10,15]  -- Should return [4,14,9,-20,15] (Odd length list, 2nd and 4th element double, neg num in list)
t7e = doubleEveryOther [7,9]           -- Should return [7,18] (Even length list, 2nd element doubled)
t7f = doubleEveryOther [7,-8,4]        -- Should return [7,-16,4] (Odd length list, 2nd element doubled, negative num in list)

-------------------------------------------------------------
-- PROBLEM #8
-- Purpose:
--    addDigits lst
--       This function takes a list of integers (with the presumption that each integer is
--       one or two digits long), and sums all the integers in such a way that when there is
--       a two digit integer, it treats both the 10s and 1s place as both 1s place integers.
--       (ie. 15 + 8 => 1 + 5 + 8)

-- Definition:
addDigits :: [Integer] -> Integer
addDigits [] = 0
addDigits (x:xs) = (div x 10) + (mod x 10) + addDigits xs


-- Tests:
--   Tests include an empty list, a list with length of 1,  a couple lists with odd lengths, and 
--   a couple lists with even lengths, a list with no 2-digit numbers, and a list with all 2-digit numbers

t8a = addDigits []              -- Should return 0 (Empty list, returns 0, nothing to add)
t8b = addDigits [15]            -- Should return 6 (List with one 2 digit element, 1+5)
t8c = addDigits [3,16,8,25]     -- Should return 25 (list w even length, 3+1+6+8+2+5)
t8d = addDigits [4,17,9,10,15]  -- Should return 28 (list with odd length, 4+1+7+9+1+0+1+5)
t8e = addDigits [7,6,0]         -- Should return 13 (no 2-digit numbers, 7+6)
t8f = addDigits [17,18,14]      -- Should return 22 (all 2-digit numbers, 1+7+8+1+4)

-------------------------------------------------------------
-- PROBLEM #9
-- Purpose:
--    luhnValidate n
--        This function takes an integer and validates it in such a way that the
--        integer is broken down into digits, and every digit is double starting with
--        the second integer from the right. Each number is then added in the same way
--        as defined in problem 8.  If the result of that sum is divisible by 10, this function
--        will return True, else will return False.

-- Definition:
luhnValidate :: Integer -> Bool
luhnValidate n
    | n < 0 = False
    | n == 0 = True
    | otherwise     = (mod sum 10) == 0
    where
      lst = doubleEveryOther (toDigitsRev n)
      sum = addDigits lst

-- Tests:
--   Tests include a negative integer, a 1-digit integer which should both always be false. Tests also
--   include integers with both an even and odd amount of digits.  Tests include validating an integer with
--   a checksum value of 0. For each true test, there will be 2 similar tests with only the checksum digit changed.

t9a = luhnValidate (-10)              -- Should return False (negative number, returning false for neg nums)
t9b = luhnValidate 0                  -- Should return True (zero, true because mod 0 10 == 0)
t9c = luhnValidate 6                  -- Should return False (No 1 digit can be divisible by 10)
t9d = luhnValidate 4896437            -- Should return True (odd amt of digits, checksum of 7)
t9e = luhnValidate 4896436            -- Should return False (same as t9c, but different/wrong checksum)
t9f = luhnValidate 4896439            -- Should return False (same as t9c, but different/wrong checksum)
t9g = luhnValidate 835330             -- Should return True (even amt of digits, checksum of 0)
t9h = luhnValidate 835336             -- Should return False (same as t9f, but different/wrong checksum)
t9i = luhnValidate 835332             -- Should return False (same as t9f, but different/wrong checksum)
 