/* ========================================================================== *
 *                                                                            *
 *                                  Evil ML                                   *
 *                                                                            *
 *                 Compiler from ML to C++ template language                  *
 *                                                                            *
 * ========================================================================== */

template <bool b> struct __ml_bool {
  static const int tag = -1;
  static const bool val = b;
};

template <char c> struct __ml_char {
  static const int tag = -1;
  static const char val = c;
};

template <int n> struct __ml_int {
  static const int tag = -1;
  static const int val = n;
};

template <bool, class T, class F>
struct __ml_if {
  static const int tag = 0;
  typedef T type;
};

template <class T, class F>
struct __ml_if <false, T, F> {
  static const int tag = 0;
  typedef F type;
};

template <class x, class y>
struct __ml_pair {
  static const int tag = 1;
  typedef x fst;
  typedef y snd;
};

// Polymorphic comparison

template <class x, class y>
struct __ml_compare {
private:
  template <class, int diff, int>
  struct aux // x::tag != y::tag
    : public __ml_int<diff> {};

  template <class dummy>
  struct aux <dummy, 0, -1> // x::tag == y::tag && tag == [boxed val]
    : public __ml_int<(x::val > y::val ? 1 : (x::val < y::val ? -1 : 0))> {};

  template <class dummy>
  struct aux <dummy, 0, 0> // x::tag == y::tag && tag == [ret val]
    : public __ml_int<__ml_compare<typename x::type, typename y::type>::val> {};

  template <class dummy, int n>
  class aux <dummy, 0, n> // x::tag == y::tag && tag == [some constructor]
  {
  private:
    static const int tmp = __ml_compare<typename x::fst, typename y::fst>::val;
  public:
    static const int val = (tmp != 0 ? tmp : __ml_compare<typename x::snd, typename y::snd>::val);
  };
public:
  static const int tag = -1;
  static const int val = aux<void, x::tag - y::tag, x::tag>::val;
};

template <class x, class y>
struct __ml_eq : public __ml_bool<__ml_compare<x, y>::val == 0> {};

template <class x, class y>
struct __ml_ne : public __ml_bool<__ml_compare<x, y>::val != 0> {};

template <class x, class y>
struct __ml_ge : public __ml_bool<__ml_compare<x, y>::val >= 0> {};

template <class x, class y>
struct __ml_le : public __ml_bool<__ml_compare<x, y>::val <= 0> {};

template <class x, class y>
struct __ml_gt : public __ml_bool<(__ml_compare<x, y>::val > 0)> {};

template <class x, class y>
struct __ml_lt : public __ml_bool<(__ml_compare<x, y>::val < 0)> {};

// conversion from lists into built-in C arrays

template <class T, class x>
class __ml_array_of_list {
private:
  template <class, bool>
  struct aux { // __ml_nil
    static inline void set (T *) {
      return;
    }
  };
  template <class dummy>
  struct aux <dummy, true> { // __ml_cons
    static inline void set (T * p) {
      *p = x::fst::val;
      __ml_array_of_list<T, typename x::snd>::set(p+1);
      return;
    }
  };
public:
  static inline void set (T * p) {
    aux<void, x::tag == 478463344>::set(p);
    return;
  }
};

// Built-in functions

struct __ml_succ {
  template <class n>
  struct fun {
    typedef __ml_int<n::val + 1> type;
  };
};

struct __ml_pred {
  template <class n>
  struct fun {
    typedef __ml_int<n::val - 1> type;
  };
};

struct __ml_min {
  template <class x, class y>
  struct fun {
    typedef __ml_int<(x::val < y::val ? x::val : y::val)> type;
  };
};

struct __ml_max {
  template <class x, class y>
  struct fun {
    typedef __ml_int<(x::val > y::val ? x::val : y::val)> type;
  };
};

struct __ml_int_of_char {
  template <class x>
  struct fun {
    typedef __ml_char<x::val> type;
  };
};

struct __ml_char_of_int {
  template <class x>
  struct fun {
    typedef __ml_int<x::val> type;
  };
};

// End of evilml.hpp
////////////////////////////////////////////////////////////////////////////////
