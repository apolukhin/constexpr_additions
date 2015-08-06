/*
 * (C) Copyright 2015 Antony Polukhin.
 *
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 */



// This file is a concept proof for making constexpr compatible std::reverse_iterator, std::move_iterator, range access free functions, iterators related free functions and std::array .
// This proposal superseeds http://cplusplus.github.io/LWG/lwg-active.html#2443

// Tested on GCC 5.2 with and CLANG 3.6. Works well.

// Macro to explicitly mark constexprs that are proposed for addition
#define PROPOSED_CONSTEXPR constexpr

#include <cstddef>
#include <iterator>

namespace example {

//////////////////////////////// reverse_iterator ///////////////////////////////////////////////////////

template <class It>
class reverse_iterator
    : public std::iterator<typename std::iterator_traits<It>::iterator_category,
                      typename std::iterator_traits<It>::value_type,
                      typename std::iterator_traits<It>::difference_type,
                      typename std::iterator_traits<It>::pointer,
                      typename std::iterator_traits<It>::reference>
{
protected:
    It current;

public:
    typedef It                                            iterator_type;
    typedef typename std::iterator_traits<It>::difference_type difference_type;
    typedef typename std::iterator_traits<It>::reference       reference;
    typedef typename std::iterator_traits<It>::pointer         pointer;

    PROPOSED_CONSTEXPR reverse_iterator()
        : current()
    {}

    PROPOSED_CONSTEXPR explicit reverse_iterator(It lhs) 
        : current(lhs) 
    {}

    template <class _Up> 
    PROPOSED_CONSTEXPR reverse_iterator(const reverse_iterator<_Up>& it)
        : current(it.base())
    {}

    PROPOSED_CONSTEXPR It base() const {
        return current;
    }

    PROPOSED_CONSTEXPR reference         operator*() const {
        It tmp = current;
        return *--tmp;
    }

    // Requires http://cplusplus.github.io/LWG/lwg-active.html#2296 to make constexpt
    /*PROPOSED_CONSTEXPR*/ pointer       operator->() const {
        return std::addressof(operator*());
    }

    PROPOSED_CONSTEXPR reverse_iterator& operator++() {
        --current;
        return *this;
    }
    PROPOSED_CONSTEXPR reverse_iterator  operator++(int) {
        reverse_iterator tmp(*this);
        --current;
        return tmp;
    }

    PROPOSED_CONSTEXPR reverse_iterator& operator--() {
        ++current;
        return *this;
    }

    PROPOSED_CONSTEXPR reverse_iterator  operator--(int) {
        reverse_iterator tmp(*this);
        ++current;
        return tmp;
    }

    PROPOSED_CONSTEXPR reverse_iterator  operator+ (difference_type n) const {
        return reverse_iterator(current - n);
    }

    PROPOSED_CONSTEXPR reverse_iterator& operator+=(difference_type n) {
        current -= n;
        return *this;
    }

    PROPOSED_CONSTEXPR reverse_iterator  operator- (difference_type n) const {
        return reverse_iterator(current + n);
    }

    PROPOSED_CONSTEXPR reverse_iterator& operator-=(difference_type n) {
        current += n;
        return *this;
    }

    PROPOSED_CONSTEXPR reference         operator[](difference_type n) const {
        return current[-n-1];
    }
};


template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator==(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() == rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator<(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() > rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator!=(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() != rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator>(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() < rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator>=(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() <= rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator<=(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs) {
    return lhs.base() >= rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR typename reverse_iterator<It1>::difference_type
    operator-(const reverse_iterator<It1>& lhs, const reverse_iterator<It2>& rhs)
{
    return rhs.base() - lhs.base();
}

template <class It>
inline PROPOSED_CONSTEXPR reverse_iterator<It> operator+(typename reverse_iterator<It>::difference_type n, const reverse_iterator<It>& lhs) {
    return reverse_iterator<It>(lhs.base() - n);
}

template <class It>
inline PROPOSED_CONSTEXPR reverse_iterator<It> make_reverse_iterator(It it) {
    return reverse_iterator<It>(it);
}

} // namespace example

//////////////////////////////// move_iterator ///////////////////////////////////////////////////////

namespace example {

template <class It>
class move_iterator : public std::iterator<typename std::iterator_traits<It>::iterator_category,
                      typename std::iterator_traits<It>::value_type&&,
                      typename std::iterator_traits<It>::value_type&&,
                      typename std::iterator_traits<It>::pointer,
                      typename std::iterator_traits<It>::reference>
{
protected:
    It current;

public:
    typedef It                                            iterator_type;
    typedef typename std::iterator_traits<It>::difference_type difference_type;
    typedef typename std::iterator_traits<It>::value_type&&       reference;
    typedef typename std::iterator_traits<It>::pointer         pointer;

    PROPOSED_CONSTEXPR move_iterator()
        : current()
    {}

    PROPOSED_CONSTEXPR explicit move_iterator(It lhs) 
        : current(lhs) 
    {}

    template <class _Up> 
    PROPOSED_CONSTEXPR move_iterator(const move_iterator<_Up>& it)
        : current(it.base())
    {}

    PROPOSED_CONSTEXPR It base() const {
        return current;
    }

    PROPOSED_CONSTEXPR reference         operator*() const {
        return std::move(*current);
    }

    // Requires http://cplusplus.github.io/LWG/lwg-active.html#2296 to make constexpt
    /*PROPOSED_CONSTEXPR*/ pointer       operator->() const {
        return std::addressof(operator*());
    }

    PROPOSED_CONSTEXPR move_iterator& operator++() {
        ++current;
        return *this;
    }
    PROPOSED_CONSTEXPR move_iterator  operator++(int) {
        move_iterator tmp(*this);
        ++current;
        return tmp;
    }

    PROPOSED_CONSTEXPR move_iterator& operator--() {
        --current;
        return *this;
    }

    PROPOSED_CONSTEXPR move_iterator  operator--(int) {
        move_iterator tmp(*this);
        --current;
        return tmp;
    }

    PROPOSED_CONSTEXPR move_iterator  operator+ (difference_type n) const {
        return move_iterator(current + n);
    }

    PROPOSED_CONSTEXPR move_iterator& operator+=(difference_type n) {
        current += n;
        return *this;
    }

    PROPOSED_CONSTEXPR move_iterator  operator- (difference_type n) const {
        return move_iterator(current - n);
    }

    PROPOSED_CONSTEXPR move_iterator& operator-=(difference_type n) {
        current -= n;
        return *this;
    }

    PROPOSED_CONSTEXPR reference         operator[](difference_type n) const {
        return std::move(current[n]);
    }
};


template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator==(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() == rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator<(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() > rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator!=(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() != rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator>(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() < rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator>=(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() <= rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR bool operator<=(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs) {
    return lhs.base() >= rhs.base();
}

template <class It1, class It2>
inline PROPOSED_CONSTEXPR typename move_iterator<It1>::difference_type
    operator-(const move_iterator<It1>& lhs, const move_iterator<It2>& rhs)
{
    return lhs.base() - rhs.base();
}

template <class It>
inline PROPOSED_CONSTEXPR move_iterator<It> operator+(typename move_iterator<It>::difference_type n, const move_iterator<It>& lhs) {
    return move_iterator<It>(lhs.base() + n);
}

template <class It>
inline PROPOSED_CONSTEXPR move_iterator<It> make_move_iterator(It it) {
    return move_iterator<It>(it);
}

} // namespace example

//////////////////////////////// range access ///////////////////////////////////////////////////////

#include <initializer_list>

namespace example {
    // 24.7, range access:
    template <class C>              PROPOSED_CONSTEXPR auto begin(C& c) -> decltype(c.begin())                          { return c.begin(); }
    template <class C>              PROPOSED_CONSTEXPR auto begin(const C& c) -> decltype(c.begin())                    { return c.begin(); }
    template <class C>              PROPOSED_CONSTEXPR auto end(C& c) -> decltype(c.end())                              { return c.end(); }
    template <class C>              PROPOSED_CONSTEXPR auto end(const C& c) -> decltype(c.end())                        { return c.end(); }
    template <class T, size_t N>    constexpr T*            begin(T (&array)[N]) noexcept                               { return array; }
    template <class T, size_t N>    constexpr T*            end(T (&array)[N]) noexcept                                 { return array + N; }
    template <class C>              constexpr auto          cbegin(const C& c) noexcept(noexcept(example::begin(c))) -> decltype(example::begin(c)) { return example::begin(c); }
    template <class C>              constexpr auto          cend(const C& c) noexcept(noexcept(example::end(c))) -> decltype(example::end(c)) { return example::end(c); }
    template <class C>              PROPOSED_CONSTEXPR auto rbegin(C& c) -> decltype(c.rbegin())                        { return c.rbegin(); }
    template <class C>              PROPOSED_CONSTEXPR auto rbegin(const C& c) -> decltype(c.rbegin())                  { return c.rbegin(); }
    template <class C>              PROPOSED_CONSTEXPR auto rend(C& c) -> decltype(c.rend())                            { return c.rend(); }
    template <class C>              PROPOSED_CONSTEXPR auto rend(const C& c) -> decltype(c.rend())                      { return c.rend(); }
    template <class T, size_t N>    PROPOSED_CONSTEXPR reverse_iterator<T*>         rbegin(T (&array)[N])               { return reverse_iterator<T*>(example::end(array)); }
    template <class T, size_t N>    PROPOSED_CONSTEXPR reverse_iterator<T*>         rend(T (&array)[N])                 { return reverse_iterator<T*>(example::begin(array)); }
    template <class E>              PROPOSED_CONSTEXPR reverse_iterator<const E*>   rbegin(std::initializer_list<E> il) { return reverse_iterator<const E*>(il.end()); }
    template <class E>              PROPOSED_CONSTEXPR reverse_iterator<const E*>   rend(std::initializer_list<E> il)   { return reverse_iterator<const E*>(il.begin()); }
    template <class C>              PROPOSED_CONSTEXPR auto crbegin(const C& c) -> decltype(example::rbegin(c))         { return example::rbegin(c); }
    template <class C>              PROPOSED_CONSTEXPR auto crend(const C& c) -> decltype(example::rend(c))             { return example::rend(c); }

//////////////////////////////// distance ///////////////////////////////////////////////////////
namespace detail {
    template <typename It>
    PROPOSED_CONSTEXPR inline typename std::iterator_traits<It>::difference_type distance_impl(It first, It last, std::input_iterator_tag) {
        typename std::iterator_traits<It>::difference_type ret = 0;
        while (first != last) {
            ++first;
            ++ret;
        }

        return ret;
    }

    template <typename It>
    PROPOSED_CONSTEXPR inline typename std::iterator_traits<It>::difference_type distance_impl(It first, It last, std::random_access_iterator_tag) {
        return last - first;
    }
} // namespace detail

    template <typename It>
    PROPOSED_CONSTEXPR inline typename std::iterator_traits<It>::difference_type distance(It first, It last) {
        return detail::distance_impl(first, last, typename std::iterator_traits<It>::iterator_category());
    }

//////////////////////////////// advance ///////////////////////////////////////////////////////
namespace detail {
    template<typename It, typename Distance>
    PROPOSED_CONSTEXPR inline void advance_impl(It& it, Distance n, std::input_iterator_tag) {
        assert(n >= 0);
        while (n--) {
            ++it;
        }
    }

    template<typename It, typename Distance>
    PROPOSED_CONSTEXPR inline void advance_impl(It& it, Distance n, std::bidirectional_iterator_tag) {
        if (n > 0) while (n--) ++it;
        else while (n++) --it;
    }

    template<typename It, typename Distance>
    PROPOSED_CONSTEXPR inline void advance_impl(It& it, Distance n, std::random_access_iterator_tag) {
        it += n;
    }
} // namespace detail

    template<typename It, typename Distance>
    PROPOSED_CONSTEXPR inline void advance(It& it, Distance n) {
        typename std::iterator_traits<It>::difference_type d = n;
        detail::advance_impl(it, d, typename std::iterator_traits<It>::iterator_category());
    }
//////////////////////////////// next ///////////////////////////////////////////////////////
    template<typename It>
    PROPOSED_CONSTEXPR inline It next(It it, typename std::iterator_traits<It>::difference_type n = 1) {
        example::advance(it, n);
        return it;
    }

//////////////////////////////// prev ///////////////////////////////////////////////////////
    template<typename It>
    PROPOSED_CONSTEXPR inline It prev(It it, typename std::iterator_traits<It>::difference_type n = 1) {
        advance(it, -n);
        return it;
    }

} // namespace example


#include <cstddef>
#include <stdexcept>
#include <cassert>
#include <algorithm>
#include <type_traits>

namespace example {

//////////////////////////////// array ///////////////////////////////////////////////////////

namespace detail {
    template <class T, std::size_t N>
    struct deduce_array_type {
        typedef T type[N];
    };

    template <class T>
    struct t_nullptr {
        constexpr operator T*() const {
            return nullptr;
        }
    };

    template <class T>
    struct deduce_array_type<T, 0> {
        typedef t_nullptr<T> type;
    };
}

template<class T, std::size_t N>
class array {
  public:
    typename detail::deduce_array_type<T, N>::type elems;    // fixed-size array of elements of type T

  public:
    // type definitions
    typedef T&             reference;
    typedef const T&       const_reference;
    typedef T*             iterator;
    typedef const T*       const_iterator;
    typedef std::size_t    size_type;
    typedef std::ptrdiff_t difference_type;
    typedef T              value_type;
    typedef T*             pointer;
    typedef const T*       const_pointer;
    typedef ::example::reverse_iterator<iterator> reverse_iterator;
    typedef ::example::reverse_iterator<const_iterator> const_reverse_iterator;

    // no explicit construct/copy/destroy for aggregate type

    void fill   (const T& value) {
        std::fill_n(begin(),size(),value);
    }

    void swap (array<T,N>& y) noexcept(!N || noexcept(std::swap(std::declval<T&>(), std::declval<T&>()))) {
        for (size_type i = 0; i < N; ++i)
            std::swap(elems[i],y.elems[i]);
    }

    // iterators:
    PROPOSED_CONSTEXPR  iterator                begin()       noexcept  { return elems; }
    PROPOSED_CONSTEXPR  const_iterator          begin() const noexcept  { return elems; }
    PROPOSED_CONSTEXPR  iterator                end()         noexcept  { return elems+N; }
    PROPOSED_CONSTEXPR  const_iterator          end()   const noexcept  { return elems+N; }

    PROPOSED_CONSTEXPR reverse_iterator         rbegin() noexcept       { return reverse_iterator(end());}
    PROPOSED_CONSTEXPR const_reverse_iterator   rbegin() const noexcept { return const_reverse_iterator(end()); }
    PROPOSED_CONSTEXPR reverse_iterator         rend() noexcept         { return reverse_iterator(begin()); }
    PROPOSED_CONSTEXPR const_reverse_iterator   rend() const noexcept   { return const_reverse_iterator(begin()); }

    PROPOSED_CONSTEXPR  const_iterator          cbegin() const noexcept { return elems; }
    PROPOSED_CONSTEXPR  const_iterator          cend()   const noexcept { return elems+N; }
    PROPOSED_CONSTEXPR  const_reverse_iterator  crbegin() const noexcept { return const_reverse_iterator(end()); }
    PROPOSED_CONSTEXPR  const_reverse_iterator  crend() const noexcept  { return const_reverse_iterator(begin()); }

    // capacity:
    constexpr bool          empty()     const noexcept { return !!N; }
    constexpr size_type     size()      const noexcept { return N; }
    constexpr size_type     max_size()  const noexcept { return N; }

    // element access
    PROPOSED_CONSTEXPR reference operator[](size_type i) {
        assert( i < N && "out of range" );
        return elems[i];
    }
    constexpr const_reference operator[](size_type i) const {
        assert( i < N && "out of range" );
        return elems[i];
    }

    PROPOSED_CONSTEXPR reference at(size_type i) {
        rangecheck(i);
        return elems[i];
    }
    constexpr const_reference at(size_type i) const {
        rangecheck(i);
        return elems[i];
    }

    PROPOSED_CONSTEXPR reference front() noexcept {
        return elems[0]; 
    }
    
    constexpr const_reference front() const noexcept {
        return elems[0];
    }

    PROPOSED_CONSTEXPR reference back() noexcept {
        return elems[N-1]; 
    }
    
    constexpr const_reference back() const noexcept {
        return elems[N-1]; 
    }

    PROPOSED_CONSTEXPR  T*          data() noexcept         { return elems; }
    constexpr           const T*    data() const noexcept   { return elems; }

    // check range (may be private because it is static)
    static constexpr bool rangecheck (size_type i) {
        return i > N ? (throw std::out_of_range ("array<>: index out of range")), true : true;
    }
};

template <size_t Idx, typename T, size_t N>
constexpr T& get(array<T,N>& arr) noexcept {
    static_assert( Idx < N, "get<>(array &) index out of range" );
    return arr[Idx];
}

template <size_t Idx, typename T, size_t N>
constexpr T&& get(array<T,N>&& arr) noexcept {
    static_assert( Idx < N, "get<>(array &) index out of range" );
    return std::move(arr[Idx]);
}

template <size_t Idx, typename T, size_t N>
constexpr const T& get(const array<T,N> &arr) noexcept {
    static_assert( Idx < N, "get<>(const array &) index out of range" );
    return arr[Idx];
}


////////////////////////////////// tests ////////////////////////////////////////////////////////////////////////

template <class T>
constexpr auto from_end(const T& container, std::size_t num) {
    return *(container.rbegin() + num);
}

template <class Conatiner, class T>
constexpr auto constexpr_find(const Conatiner& c, T value) {
    auto begin = example::begin(c);
    while (begin != c.end()) {
        if (*begin == value) break;
        
        ++begin;
    }
    return *begin;
}

template <class Conatiner, class T>
constexpr auto constexpr_rfind(const Conatiner& c, T value) {
    auto begin = example::rbegin(c);
    auto end = example::rend(c);
    while (begin != end) {
        if (*begin == value) break;
        
        example::next(begin);
    }
    return *begin;
}

template <class Conatiner>
constexpr auto constexpr_rbuble_sort(const Conatiner& c) {
    auto ret = c;
    // CLANG 3.6 compiles the following line, GCC 5.2 fails
    // const std::size_t size = ::example::distance(rbegin(ret), rend(ret));
    const std::size_t size = ret.size();
    for (std::size_t i = 0; i < size; ++i) {
        for (auto it = rbegin(ret); it != rend(ret) - 1 - i; ::example::advance(it, 1)) {
            if (*it > *(it + 1)) {
                auto tmp = *::example::make_move_iterator(it);
                *it = *::example::make_move_iterator(it + 1);
                *(it + 1) = tmp;
            }
        }
    }
    return ret;
}

constexpr array<int, 10> constexpr_test_function(const array<int, 10>& in) {
    array<int, 10> res = in;
    res.front() = res.back();
    res[2] = res.at(res.size() - 2);
    res[3] = *(res.data() + res[4]);
    get<4>(res) = get<5>(res);

    auto it0 = res.rbegin();
    example::advance(it0, 5);
    auto it1 = res.rend();
    example::advance(it1, -5);
    *it0 = *it1;

    return res;
}

constexpr bool is_equal_arrays(const array<int, 10>& lhs, const array<int, 10>& rhs) {
    for (array<int, 10>::size_type i = 0; i < 10; ++i) {
        if (lhs[i] != rhs[i]) {
            return false;
        }
    }

    return true;
}

} // namespace example

#include <iostream>

int main() {
    using namespace example;

    { // Zero sized array test
        constexpr array<int, 0> a_zero{};
        static_assert(a_zero.begin() == a_zero.end(), "");
        static_assert(a_zero.rbegin() == a_zero.rend(), "");
        static_assert(a_zero.cbegin() == a_zero.cend(), "");
        static_assert(a_zero.crbegin() == a_zero.crend(), "");

      // Zero sized array test with range access functions
        static_assert(begin(a_zero) == end(a_zero), "");
        static_assert(rbegin(a_zero) == rend(a_zero), "");
        static_assert(cbegin(a_zero) == cend(a_zero), "");
        static_assert(crbegin(a_zero) == crend(a_zero), "");
    }

    { // Array constexpr functionality tests
        constexpr array<int, 10> cx14 {{ 0,1,2,3,4,5,6,7,8,9 }};
        constexpr array<int, 10> res_array = constexpr_test_function(cx14);
        constexpr array<int, 10> ethalon {{ 9,1,8,4,5,5,6,7,8,9 }};
        // We can not use `operator ==` with constexpr because it uses std::equal
        static_assert(
            is_equal_arrays(res_array, ethalon),
            "Failed static assert with ethalon value"
        );

        constexpr array<int, 5> a0 {{1, 2, 3, 4, 5}};
        constexpr auto r = from_end(a0, 1);
        static_assert(r == 4, "");
        
        constexpr array<int, 15> a1 {{1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5}};
        constexpr auto bw = constexpr_find(a1, 5);
        static_assert(bw == 5, "");

      // Array test with range access functions
        static_assert(begin(cx14) + cx14.size() == end(cx14), "");
        static_assert(rbegin(cx14) != rend(cx14), "");
        static_assert(cbegin(cx14) + cx14.size() == cend(cx14), "");
        static_assert(crbegin(cx14) != crend(cx14), "");
        static_assert(*begin(cx14) == *cbegin(cx14), "");
    }

    { // reverse_iterator functionality test
        constexpr array<int, 15> a1 {{1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5}};
        static_assert(a1.begin() == a1.rend().base(), "");
        static_assert(a1.rbegin().base() == a1.end(), "");
        static_assert(a1.cbegin() == a1.crend().base(), "");
        static_assert(a1.crbegin().base() == a1.cend(), "");
        static_assert(a1.begin() + 5 == (a1.rbegin() + 10).base(), "");

        constexpr auto fw = constexpr_rfind(a1, 5);
        static_assert(fw == 5, "");
        
        constexpr auto res = constexpr_rbuble_sort(a1);
        for (auto a: res) {
            std::cerr << a << "  ";
        }
    }

    { // move_iterator functionality test
        constexpr array<int, 15> a1 {{1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5}};
        static_assert(make_move_iterator(a1.begin()) == make_move_iterator(a1.rend().base()), "");
        static_assert(make_move_iterator(a1.rbegin().base()) == make_move_iterator(a1.end()), "");
        static_assert(make_move_iterator(a1.begin()) + 5 == make_move_iterator((a1.rbegin() + 10).base()), "");
        static_assert(make_move_iterator(a1.begin()) + 5 == make_move_iterator((a1.rbegin() + 10).base()), "");
    }

    std::cerr << "\nOK\n";
}


