#pragma once

#include <memory>

namespace cinnabar {
template <typename T, class A = std::allocator<T>>
struct value_ptr {
  typename std::allocator_traits<A>::pointer *p;
  A allocator;

public:
  value_ptr(std::nullptr_t = nullptr) : p(nullptr), allocator(std::allocator<T>{}) {}

  value_ptr(const T &value): value_ptr(value, std::allocator<T>{}) {}

  value_ptr(const T &value, A a)
      : p(std::allocator_traits<A>::allocate(a, sizeof(T))), allocator(a) {
        std::allocator_traits<A>::construct(allocator, value);
  }

  value_ptr(const value_ptr &other): value_ptr(other, std::allocator<T>{}) {}
  
  value_ptr(const value_ptr &other, A a): p(nullptr), allocator(a) {
    if (other) {
      p = std::allocator_traits<A>::allocate(allocator, sizeof(T));
      std::allocator_traits<A>::construct(allocator, *other);
    }
  }

  value_ptr(const value_ptr &&other) noexcept: value_ptr(other, std::allocator<T>{}) {}
  
  value_ptr(const value_ptr &&other, A a) noexcept: p(nullptr), allocator(a) {
    other.swap(*this);
  }

  // copy-swap
  value_ptr &operator=(value_ptr other) {
    swap(*this, other);
    return *this;
  }

  value_ptr(value_ptr &&other) noexcept : p(nullptr) { other.swap(*this); }
  value_ptr &operator=(value_ptr &&other) noexcept {
    other.swap(*this);
    return *this;
  }
  
  T *get() noexcept { return p; }
  const T *get() const noexcept { return p; }

  T &operator*() { return *p; }
  const T& operator*() const { return *p; }

  T *const operator->() noexcept { return p; }
  const T *const operator->() const noexcept { return p; }
  
  ~value_ptr() {
    if (p) {
      std::allocator_traits<A>::destroy(allocator, p);
      std::allocator_traits<A>::deallocate(allocator, p, 1);
    }
  }

  void swap(value_ptr &other) noexcept {
    using std::swap;
    swap(*this->p, other.p);
  }

  explicit operator bool() const { return p; }
};

template <typename T>
void swap(value_ptr<T> &lhs, value_ptr<T> &rhs) noexcept {
  lhs.swap(rhs);
}

} // namespace cinnabar