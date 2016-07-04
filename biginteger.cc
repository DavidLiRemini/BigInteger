#include <stdlib.h>
#include <math.h>
#include <algorithm>
#include <vector>
#include <iostream>
#include <boost/timer/timer.hpp>
class BigInteger
{
public:
	BigInteger(long long lhs)
	{
		long long va = lhs;
		if (lhs > 0)
		{
			_signed = 1;
		}
		else
			_signed = -1;
		for (; va != 0; va /= base)
		{
			innerData.push_back(va % base);
		}
	}
	BigInteger() = default;
	BigInteger GetNbitsLength(int n, BigInteger& symbol)const;
	int GetSignProperty()const
	{
		return _signed;
	}
	void SetSignProperty(int sn)
	{
		_signed = sn;
	}
private:
	friend BigInteger operator+(const BigInteger&, const BigInteger&);
	friend std::ostream& operator<<(std::ostream& out, const BigInteger&);
	friend BigInteger operator-(const BigInteger&, const BigInteger&);
	friend BigInteger operator-(const BigInteger&);
	friend BigInteger operator*(const BigInteger&, const BigInteger&);
	friend BigInteger operator/(const BigInteger&, const BigInteger&);
	friend BigInteger operator%(const BigInteger&, const BigInteger&);
	friend int AbsCompare(const BigInteger& lhs, const BigInteger& rhs);
	friend void DivRem(const BigInteger& dividend, const BigInteger& divisor, BigInteger& quotient, BigInteger& remainder);
	static const int base = 10;
	int _signed;
	std::vector<int>innerData;
};

BigInteger BigInteger::GetNbitsLength(int n, BigInteger& rhs)const
{
	BigInteger result;
	int remainer = innerData.size() - n;
	for (int i = 0; i < innerData.size(); ++i)
	{
		if (i < remainer)
		{
			result.innerData.push_back(0);
		}
		else
		{
			result.innerData.push_back(innerData.at(i));
			rhs.innerData.push_back(innerData.at(i));
		}
	}
	result._signed = _signed;
	rhs._signed = _signed;
	return result;
}

BigInteger operator-(const BigInteger& ele)
{
	BigInteger cpEle = ele;
	cpEle._signed = -cpEle._signed;
	return cpEle;
}
std::ostream& operator<<(std::ostream& out, const BigInteger& rhs)
{
	auto iter = rhs.innerData.crbegin();
	if (rhs.GetSignProperty() < 0)
	{
		out << "-";
	}
	for (; iter != rhs.innerData.crend(); iter++)
	{
		out << *iter;
	}
	out << '\n';
	return out;
}

BigInteger operator+ (const BigInteger& lhs, const BigInteger& rhs)
{
	BigInteger result;
	int realSymbol = lhs._signed * rhs._signed;
	int index = 0;
	int len1 = lhs.innerData.size();
	int len2 = rhs.innerData.size();
	int compLen = len1 > len2 ? len2 : len1;
	std::vector<int>resultSet(len1 + len2+1);
	int carry = 0;
	if (realSymbol >= 0)//同号
	{
		int i = 0;
		for (; i < compLen; ++i)
		{
			long absValue = lhs.innerData.at(i) + rhs.innerData.at(i)+carry;
			resultSet[index] = absValue % BigInteger::base;
			carry = absValue / BigInteger::base;
			index++;
		}
		if (compLen == len1)//lhs.list
		{
			for (int j = i; j < len2; ++j)
			{
				long value = rhs.innerData.at(j) + carry;
				resultSet[index++] = value % BigInteger::base;
				carry = value / BigInteger::base;
			}
		}
		else
		{
			for (int j = i; j < len1; ++j)
			{
				long value = lhs.innerData.at(j) + carry;
				resultSet[index++] = value % BigInteger::base;
				carry = value / BigInteger::base;
			}
		}
		if (carry != 0)
		{
			resultSet[index] = carry;
			carry = 0;
		}

	}
	else
	{
		const BigInteger& bigger = AbsCompare(lhs, rhs) > 0 ? lhs : rhs;
		const BigInteger& smaller = AbsCompare(lhs,rhs) > 0 ? rhs : lhs;
		int i = 0;
		for (; i < compLen; i++)
		{
			long value = bigger.innerData.at(i) - smaller.innerData.at(i) + carry;
			if (value < 0)
			{
				carry = -1;
			}
			else
				carry = 0;
			resultSet[index++] = carry != 0 ? value + BigInteger::base : value;
		}

		if (compLen == len1)//lhs.list 比较短
		{
			for (int j = i; j < len2; ++j)
			{
				long value = rhs.innerData.at(j) + carry;
				if (value < 0)
				{
					carry = -1;
				}
				else
					carry = 0;
				resultSet[index++] = carry != 0 ? value + BigInteger::base : value;
			}
		}
		else
		{
			for (int j = i; j < len1; ++j)
			{
				long value = lhs.innerData.at(j) + carry;
				if (value < 0)
				{
					carry = -1;
				}
				else
					carry = 0;
				resultSet[index++] = carry != 0 ? value + BigInteger::base : value;
			}
		}
		if (carry != 0)
		{
			resultSet[index] = carry;
		}

	}
	result._signed = lhs._signed * (carry == 0 ? 1 : -1);
	if (realSymbol < 0)
	{
		if (lhs._signed < 0)
		{
			if (AbsCompare(lhs, rhs) > 0)
			{
				result._signed = -1;
			}
			else
				result._signed = 1;
		}
		else
		{
			if (AbsCompare(lhs, rhs) > 0)
				result._signed = 1;
			else
				result._signed = -1;
		}
	}
	result.innerData = resultSet;
	while (result.innerData.back() == 0)
	{
		result.innerData.pop_back();
	}
	return result;
}

BigInteger operator-(const BigInteger& lhs, const BigInteger& rhs)
{
	return lhs + (-rhs);
}

BigInteger operator*(const BigInteger& lhs, const BigInteger& rhs)
{
	const BigInteger& bigger = lhs.innerData.size() > rhs.innerData.size() ? lhs : rhs;
	const BigInteger& shorter = lhs.innerData.size() > rhs.innerData.size() ? rhs : lhs;

	int carry = 0;
	int signSymbol = lhs._signed * rhs._signed;
	int totalsize = lhs.innerData.size() + rhs.innerData.size() + 1;
	std::vector<BigInteger>collection;
	std::vector<int>resultSet(totalsize);
	BigInteger result;
	result._signed = signSymbol;
	int index = 0;
	int tag = 0;
	for (int i = 0; i < shorter.innerData.size(); ++i)
	{

		for (int j = 0; j < bigger.innerData.size(); ++j)
		{
			long value = (shorter.innerData.at(i) * bigger.innerData.at(j) + carry);
			resultSet[index++] = value % BigInteger::base;
			carry = value / BigInteger::base;
		}
		if (carry != 0)
		{
			resultSet[index++] = carry;
			carry = 0;
		}
		tag++;
		index = tag;
		BigInteger temp;
		temp.innerData = resultSet;
		resultSet.clear();
		resultSet.resize(totalsize);
		collection.push_back(temp);
	}
	for (const auto& it: collection)
	{
		if (result.innerData.size() == 0)
		{
			result.innerData = it.innerData;
		}
		else
			result = result + it;
	}
	while (result.innerData.back() == 0)
	{
		result.innerData.pop_back();
	}
	return result;
}

static int FindValueIndex(const BigInteger& tobeFound, BigInteger* source)
{
	int low = 0; 
	int high = 8;
	int mid = (low + high) / 2;

	if (AbsCompare(source[mid], tobeFound) > 0)
	{
		while (true)
		{
			if (mid > 0)
			{
				--mid;
				if (AbsCompare(source[mid], tobeFound) < 0 || AbsCompare(source[mid], tobeFound) == 0)
				{
					return mid;
				}
			}
			else
				break;
		}
	}
	else if (AbsCompare(source[mid], tobeFound) < 0)
	{
		while (true)
		{
			if (mid < 8)
			{
				++mid;
				if (AbsCompare(source[mid], tobeFound) > 0)
				{
					return mid - 1;
				}
				else if (!AbsCompare(source[mid], tobeFound))
					return mid;
			}
			else
				break;
		}
	}
	else
	{
		return mid;
	}
	return mid;
}

int AbsCompare(const BigInteger& lhs, const BigInteger& rhs)
{
	int len1 = lhs.innerData.size();
	int len2 = rhs.innerData.size();
	if (len1 < len2)
	{
		return -1;
	}
	else if (len2 < len1)
	{
		return 1;
	}
	else
	{
		for (int i = len1 - 1; i >= 0; --i)
		{
			if (lhs.innerData.at(i) < rhs.innerData.at(i))
			{
				return -1;
			}
			else if (lhs.innerData.at(i) > rhs.innerData.at(i))
				return 1;
		}
	}
	return 0;
}

void DivRem(const BigInteger& dividend, const BigInteger& divisor, BigInteger& quotient, BigInteger& remainder)
{
	BigInteger optVariable = dividend;
	BigInteger divisor2 = divisor;
	optVariable.SetSignProperty(1);
	divisor2.SetSignProperty(1);
	BigInteger dataMap[9] = {
		divisor2, divisor2 * BigInteger(2), divisor2 * BigInteger(3), divisor2 * BigInteger(4),
		divisor2 * BigInteger(5), divisor2 * BigInteger(6), divisor2 * BigInteger(7),
		divisor2 * BigInteger(8), divisor2 * BigInteger(9)
	};

	if (AbsCompare(dividend, divisor2) == -1)
	{
		quotient = dividend;
		return;
	}
	else
	{
		int nbits = divisor2.innerData.size();
		int attachValue = nbits;
		bool flag = false;
		while (true)
		{
			BigInteger temp;
			BigInteger symbol;
			if (optVariable.innerData.size() >= nbits)
			{
				temp = optVariable.GetNbitsLength(nbits, symbol);
			}
			else
				flag = true;
			if (flag)
			{
				break;
			}
			if (AbsCompare(symbol, divisor2) == -1 && !flag)
			{
				++nbits;
				continue;
			}
			int index = FindValueIndex(symbol, dataMap);
			quotient.innerData.push_back(index + 1);
			BigInteger tmp = dataMap[index];
			while (tmp.innerData.size() < optVariable.innerData.size())
			{
				tmp.innerData.insert(tmp.innerData.begin(), 0);
				if (AbsCompare(optVariable, tmp) < 0)
				{
					tmp.innerData.erase(tmp.innerData.begin());
					break;
				}
			}
			size_t oldSz = optVariable.innerData.size();
			optVariable = optVariable - tmp;
			if ((oldSz - optVariable.innerData.size()) == dataMap[index].innerData.size())
			{
				quotient.innerData.push_back(0);
			}
			nbits = attachValue;
		}
	}
	quotient._signed = dividend._signed * divisor._signed;
	std::reverse(quotient.innerData.begin(), quotient.innerData.end());
	if (dividend._signed < 0)
	{
		optVariable._signed = -1;
	}
	else
		optVariable._signed = 1;
	remainder = optVariable;
}

BigInteger operator/(const BigInteger& lhs, const BigInteger& rhs)
{
	BigInteger quotient;
	BigInteger remainder;
	if (rhs.innerData.size() == 0)
	{
		throw std::runtime_error("除数不能为0");
	}
	DivRem(lhs, rhs, quotient, remainder);
	assert(quotient.innerData.size() > 0);
	return quotient;
}

BigInteger operator%(const BigInteger& dividend, const BigInteger& divisor)
{
	BigInteger quotient;
	BigInteger remainder;
	DivRem(dividend, divisor, quotient, remainder);
	assert(remainder.innerData.size() >= 0);
	return remainder;
}
