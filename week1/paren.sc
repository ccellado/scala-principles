import java.security.KeyStore.TrustedCertificateEntry
def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], b: Int): Boolean = {
        if (b == 0 && chars.isEmpty)
            return true
        else if (b < 0 || (chars.isEmpty && b > 0))
            return false
        else if (chars.head == '(')
            return isBalanced(chars.tail, b + 1)
        else if (chars.head == ')')
            return isBalanced(chars.tail, b - 1)
        else
            return isBalanced(chars.tail, b)
    }
    return isBalanced(chars, 0)
}

val str = "()()()((()))"
if (balance(str.toList))
    println("Good")
else
    println("Bad")
