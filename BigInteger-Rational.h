#include <iostream>
#include <vector>
#include <string>
#include <iomanip>

const int dec_block = 10000; // размер одного блока
const int dec_sz = 4; // размер  блока в цифрах

class BigInteger {
public:
    BigInteger() : sz(0) {
        normalize();
    }
    BigInteger(long long number) {
        if (number < 0) {
            sign = 1;
            number *= -1;
        } else {
            sign = 0;
        }
        sz = 0;
        while (number > 0) {
            int cur_block = number % dec_block;
            val.push_back(cur_block);
            number /= dec_block;
            ++sz;
        }
        normalize();
    }
    BigInteger& operator=(BigInteger other) {
        normalize();
        swap(other);
        normalize();
        return *this;
    }

    explicit operator bool() const {
        return *this != 0;
    }

    std::string toString() const {
        std::string ans = "";
        if (sign) ans += '-';
        for (int i = sz - 1; i >= 0; --i) {
            int signif = val[i];
            int cnt = 0;
            if (signif == 0) cnt = 1;
            while (signif > 0) {
                cnt++;
                signif /= 10;
            }
            for (int j = 0; j < dec_sz - cnt && i < sz - 1; ++j) {
                ans += '0';
            }
            ans += std::to_string(val[i]);
        }
        return ans;
    }

    void normalize() {
        while (sz > 0 && val[sz - 1] == 0) {
            val.pop_back();
            sz--;
        }
        if (sz == 0) {
            val.push_back(0);
            sz++;
            sign = 0;
        }
    }

    void swap(BigInteger& other) {
        std::swap(val, other.val);
        std::swap(sz, other.sz);
        std::swap(sign, other.sign);
        normalize();
    }

    bool getSign() const {
        return sign;
    }

    BigInteger operator-() const {
        BigInteger temp = *this;
        temp.sign = !sign;
        temp.normalize();
        return temp;
    }

    BigInteger& operator+=(const BigInteger& other) {
        normalize();
        if (this == &other) {
            *this *= 2;
            return *this;
        }
        if (sign == other.sign) {
            long long carry = 0;
            int i = 0;
            for (; i < (sz < other.sz ? sz : other.sz); ++i) {
                val[i] += other.val[i] + carry;
                if (val[i] - dec_block >= 0) {
                    carry = val[i] / dec_block;
                    val[i] -= dec_block;
                } else {
                    carry = 0;
                }
            }
            while (i < other.sz) {
                val.push_back(other.val[i] + carry);
                if (val[i] - dec_block >= 0) {
                    carry = val[i] / dec_block;
                    val[i] -= dec_block;
                } else {
                    carry = 0;
                }
                ++i;
                ++sz;
            }
            while (i < sz) {
                val[i] += carry;
                if (val[i] - dec_block >= 0) {
                    carry = val[i] / dec_block;
                    val[i] -= dec_block;
                } else {
                    carry = 0;
                }
                ++i;
            }
            if (carry > 0) {
                val.push_back(carry);
                ++sz;
            }
            normalize();
            return *this;
        } else if (sign == 1) {
            BigInteger temp = other;
            temp -= -(*this);
            *this = temp;
            normalize();
            return *this;
        } else {
            *this -= -other;
            normalize();
            return *this;
        }
    }

    BigInteger operator-=(const BigInteger& other) {
        normalize();
        if (this == &other) {
            *this = 0;
            return *this;
        }
        if (sign == other.sign) {
            if (*this >= other) {
                long long carry = 0;
                int i = 0;
                for (; i < other.sz; ++i) {
                    val[i] -= other.val[i] + carry;
                    if (val[i] < 0) {
                        carry = 1;
                        val[i] += dec_block;
                    } else {
                        carry = 0;
                    }
                }
                while (carry > 0 && i < sz) {
                    val[i] -= carry;
                    if (val[i] < 0) {
                        carry = 1;
                        val[i] += dec_block;
                    } else {
                        carry = 0;
                    }
                    ++i;
                }
                normalize();
                return *this;
            } else {
                BigInteger temp = other;
                temp -= *this;
                swap(temp);
                sign = !sign;
                normalize();
                return *this;
            }
        } else if (other.sign == 1) {
            normalize();
            return *this += (-other);
        } else {
            BigInteger temp = other;
            temp += -(*this);
            temp = -temp;
            *this = temp;
            normalize();
            return *this;
        }
    }

    BigInteger operator*=(const BigInteger& other) {
        normalize();
        BigInteger update;
        update.sz = sz + other.sz + 1;
        update.val.resize(update.sz);
        update.sign = (sign != other.sign);

        for (int i = 0; i < sz; ++i) {
            for (int j = 0; j < other.sz; ++j) {
                update.val[i + j] += val[i] * other.val[j];
            }
        }

        for (int i = 0; i < update.sz - 1; ++i) {
            update.val[i + 1] += update.val[i] / dec_block;
            update.val[i] %= dec_block;
        }

        update.normalize();
        *this = update;
        return *this;
    }

    BigInteger operator/=(const BigInteger& other) {
        normalize();
        if (this == &other) {
            *this = 1;
            return *this;
        }
        int i = sz - 1;
        BigInteger cur = 0;
        cur += val[i];
        BigInteger ans = 0;
        while (i >= 0) {
            int l = 0, r = dec_block;
            while (r - l > 1) {
                int m = l + (r - l) / 2;
                BigInteger update = m * (other > 0 ? other : -other);
                if (update > cur) r = m;
                else l = m;
            }
            cur -= BigInteger(l) * (other > 0 ? other : -other);
            ans *= dec_block;
            ans += l;
            --i;
            if (i >= 0) {
                cur *= dec_block;
                cur += val[i];
            }
        }
        ans.sign = (sign != other.sign);
        *this = ans;
        normalize();
        return *this;
    }

    BigInteger operator%=(const BigInteger& other) {
        normalize();
        *this -= (*this / other) * other;
        normalize();
        return *this;
    }

    BigInteger operator+(const BigInteger& other) const {
        BigInteger update = *this;
        update += other;
        return update;
    }

    BigInteger operator-(const BigInteger& other) const {
        BigInteger update = *this;
        update -= other;
        return update;
    }

    BigInteger operator*(const BigInteger& other) const {
        BigInteger update = *this;
        update *= other;
        return update;
    }

    BigInteger operator/(const BigInteger& other) const {
        BigInteger update = *this;
        update /= other;
        return update;
    }

    BigInteger operator%(const BigInteger& other) const {
        BigInteger update = *this;
        update %= other;
        return update;
    }

    BigInteger& operator++() {
        *this += 1;
        return *this;
    }

    BigInteger& operator--() {
        *this -= 1;
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger update = *this;
        ++(*this);
        return update;
    }

    BigInteger operator--(int) {
        BigInteger update = *this;
        --(*this);
        return update;
    }

    friend BigInteger operator+(int first, const BigInteger& second);
    friend BigInteger operator-(int first, const BigInteger& second);
    friend BigInteger operator*(int first, const BigInteger& second);
    friend BigInteger operator/(int first, const BigInteger& second);
    friend BigInteger operator%(int first, const BigInteger& second);
    friend bool operator<(const BigInteger& first, const BigInteger& second);
    friend bool operator>(const BigInteger& first, const BigInteger& second);
    friend bool operator<=(const BigInteger& first, const BigInteger& second);
    friend bool operator>=(const BigInteger& first, const BigInteger& second);
    friend bool operator==(const BigInteger& first, const BigInteger& second);
    friend bool operator!=(const BigInteger& first, const BigInteger& second);
    friend std::ostream& operator<<(std::ostream& out, const BigInteger& first);
    friend std::istream& operator>>(std::istream& in, BigInteger& first);
    friend BigInteger pow10(size_t exp);

private:
    std::vector<long long> val; // число, разбитое на string
    int sz; // количество блоков в val
    bool sign;

};

BigInteger operator+(int first, const BigInteger& second) {
    BigInteger update = first;
    update += second;
    return update;
}

BigInteger operator-(int first, const BigInteger& second) {
    BigInteger update = first;
    update -= second;
    return update;
}

BigInteger operator*(int first, const BigInteger& second) {
    BigInteger update = first;
    update *= second;
    return update;
}

BigInteger operator/(int first, const BigInteger& second) {
    BigInteger update = first;
    update /= second;
    return update;
}

BigInteger operator%(int first, const BigInteger& second) {
    BigInteger update = first;
    update %= second;
    return update;
}

bool operator<(const BigInteger& first, const BigInteger& second) {
    if (first.sign != second.sign) return  first.sign > second.sign;
    if (first.sz != second.sz) return first.sz < second.sz;
    for (int i = first.sz - 1; i >= 0; --i) {
        if (first.val[i] != second.val[i]) return first.val[i] < second.val[i];
    }
    return false;
}

bool operator>(const BigInteger& first, const BigInteger& second) {
    return second < first;
}

bool operator<=(const BigInteger& first, const BigInteger& second) {
    return !(first > second);
}

bool operator>=(const BigInteger& first, const BigInteger& second) {
    return second <= first;
}

bool operator==(const BigInteger& first, const BigInteger& second) {
    return first <= second && first >= second;
}

bool operator!=(const BigInteger& first, const BigInteger& second) {
    return first < second || first > second;
}

std::ostream& operator<<(std::ostream& out, const BigInteger& first) {
    out << first.toString();
    return out;
}

std::istream& operator>>(std::istream& in, BigInteger& first) {
    std::string str;
    in >> str;
    first.sign = 0;
    first.val.clear();
    first.sz = 0;
    int cnt = 0;
    int coef = 1;
    if (str[0] == '-') first.sign = 1;
    for (int i = str.size() - 1; i >= first.sign; --i) {
        if (cnt == 0) {
            first.val.push_back(str[i] - '0');
            first.sz++;
            coef = 10;
        } else {
            first.val[first.sz - 1] += (str[i] - '0') * coef;
            coef *= 10;
        }
        cnt = (cnt + 1) % dec_sz;
    }
    return in;
}

BigInteger pow10(size_t exp) {
    BigInteger ans;
    ans.val.clear();
    ans.sz = 0;
    for (size_t i = 0; i < exp / dec_sz; ++i) {
        ans.val.push_back(0);
        ans.sz++;
    }
    ans.val.push_back(1);
    ans.sz++;
    for (size_t i = 0; i < exp % dec_sz; ++i) {
        ans.val[ans.sz - 1] *= 10;
    }
    return ans;
}

BigInteger gcd(BigInteger a, BigInteger b) {
    if (a < b) return gcd(b, a);
    if (b == 0) return a;
    return gcd(a % b, b);
}

class Rational {
public:
    Rational() : sign(0), num(0), denom(1) {}
    Rational(long long number) : sign(number < 0), num(abs(number)), denom(1) {}
    Rational(long long _num, long long _denom) : sign((_num > 0) != (_denom > 0)), num(abs(_num)), denom(abs(_denom)) {
        normalize();
    }
    Rational(const BigInteger& number) : sign(number < 0), num(number > 0 ? number : -number), denom(1) {}
    Rational(const BigInteger& _num, const BigInteger& _denom) : sign((_num > 0) != (_denom > 0)), num(_num > 0 ? _num : -_num),
    denom(_denom > 0 ? _denom : -_denom) {
        normalize();
    }
    Rational& operator=(Rational other) {
        swap(other);
        return *this;
    }

    std::string toString() const {
        std::string ans = "";
        if (sign) ans += '-';
        ans += num.toString();
        if (denom != 1) {
            ans += '/';
            ans += denom.toString();
        }
        return ans;
    }

    void normalize() {
        BigInteger div = gcd(num, denom);
        num /= div;
        denom /= div;
        num.normalize();
        denom.normalize();
        num = num > 0 ? num : -num;
        denom = denom > 0 ? denom : -denom;
        if (num == 0) sign = 0;
    }

    void swap(Rational& other) {
        std::swap(num, other.num);
        std::swap(denom, other.denom);
        std::swap(sign, other.sign);
    }

    Rational operator-() const {
        Rational temp = *this;
        temp.sign = !sign;
        temp.normalize();
        return temp;
    }

    Rational& operator+=(const Rational& other) {
        BigInteger new_denom = denom * other.denom / gcd(denom, other.denom);
        if (sign == other.sign) {
            num = num * new_denom / denom + other.num * new_denom / other.denom;
            denom = new_denom;
        } else {
            num = num * new_denom / denom - other.num * new_denom / other.denom;
            denom = new_denom;
            if (num < 0) {
                sign = !sign;
                num *= -1;
            }
        }
        normalize();
        return *this;
    }

    Rational& operator-=(const Rational& other) {
        *this += -other;
        return *this;
    }

    Rational& operator*=(const Rational& other) {
        if (other.sign) sign = !sign;
        num *= other.num;
        denom *= other.denom;
        normalize();
        return *this;
    }

    Rational& operator/=(const Rational& other) {
        if (other.sign) sign = !sign;
        num *= other.denom;
        denom *= other.num;
        normalize();
        return *this;
    }

    Rational operator+(const Rational& other) const {
        Rational update = *this;
        update += other;
        return update;
    }

    Rational operator-(const Rational& other) const {
        Rational update = *this;
        update -= other;
        return update;
    }

    Rational operator*(const Rational& other) const {
        Rational update = *this;
        update *= other;
        return update;
    }

    Rational operator/(const Rational& other) const {
        Rational update = *this;
        update /= other;
        return update;
    }

    std::string asDecimal(size_t precision = 0) const {
        std::string ans = "";
        if (sign == 1) ans += '-';
        BigInteger coef = pow10(precision);
        BigInteger num_div = num * coef;
        BigInteger update = num_div / denom;
        std::string rational_str = update.toString();
        while (rational_str.size() < precision + 1) {
            rational_str = '0' + rational_str;
        }
        std::string integer_part = rational_str.substr(0, rational_str.size() - precision);
        std::string rational_part = rational_str.substr(rational_str.size() - precision, precision);
        ans += integer_part;
        if (rational_part == "") return ans;
        ans += '.';
        ans += rational_part;
        return ans;
    }

    explicit operator double() const {
        std::string rational_str = asDecimal(15);
        double integer_part = 0, rational_part = 0;
        size_t i = 0;
        while (i < rational_str.size() && rational_str[i] != '.') {
            integer_part *= 10;
            integer_part += rational_str[i] - '0';
            ++i;
        }
        if (i == rational_str.size()) return integer_part;
        ++i;
        double coef = 0.1;
        while (i < rational_str.size()) {
            rational_part += coef * (rational_str[i] - '0');
            coef *= 0.1;
            ++i;
        }
        return  integer_part + rational_part;
    }

    friend Rational operator+(int first, const Rational& second);
    friend Rational operator-(int first, const Rational& second);
    friend Rational operator*(int first, const Rational& second);
    friend Rational operator/(int first, const Rational& second);
    friend bool operator<(const Rational& first, const Rational& second);
    friend bool operator>(const Rational& first, const Rational& second);
    friend bool operator<=(const Rational& first, const Rational& second);
    friend bool operator>=(const Rational& first, const Rational& second);
    friend bool operator==(const Rational& first, const Rational& second);
    friend bool operator!=(const Rational& first, const Rational& second);
    friend std::ostream& operator<<(std::ostream& out, const Rational& first);

private:
    bool sign;
    BigInteger num;
    BigInteger denom;

};

Rational operator+(int first, const Rational& second) {
    Rational update = first;
    update += second;
    return update;
}

Rational operator-(int first, const Rational& second) {
    Rational update = first;
    update -= second;
    return update;
}

Rational operator*(int first, const Rational& second) {
    Rational update = first;
    update *= second;
    return update;
}

Rational operator/(int first, const Rational& second) {
    Rational update = first;
    update /= second;
    return update;
}

bool operator<(const Rational& first, const Rational& second) {
    if (first.sign != second.sign) return first.sign;
    return (first.num * second.denom < second.num * first.denom) != first.sign;
}

bool operator>(const Rational& first, const Rational& second) {
    return second < first;
}

bool operator<=(const Rational& first, const Rational& second) {
    return !(first > second);
}

bool operator>=(const Rational& first, const Rational& second) {
    return !(first < second);
}

bool operator==(const Rational& first, const Rational& second) {
    return first <= second && first >= second;
}

bool operator!=(const Rational& first, const Rational& second) {
    return first < second || first > second;
}

std::ostream& operator<<(std::ostream& out, const Rational& first) {
    out << first.toString();
    return out;
}

