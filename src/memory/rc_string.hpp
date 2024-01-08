#pragma once

// code borrowed from MRustC by John Hodge (Mutabah/thePowersGang)

#include <memory>
#include <string_view>

namespace cinnabar {
// reference-counted string
// as name suggests, not for multithreaded use

template <class A = std::allocator<std::byte>>
class RcString {
  struct Inner {
    uint32_t ref_count;
    uint32_t size;
    char data[1];
  };

  typename std::allocator_traits<A>::pointer data;
  A allocator;

  static_assert(
      std::is_same_v<typename A::value_type, std::byte>,
      "Allocator must allocate bytes"
  );

public:

  RcString(std::string_view sv): RcString(sv, std::allocator<std::byte>()) {}

  template<typename = std::enable_if_t<std::is_same_v<A, std::allocator<std::byte>>>>
  RcString(std::string_view sv, A a) : data(nullptr), allocator(a) {
    if (sv.size() > 0) {
      data = std::allocator_traits<A>::allocate(
          allocator, sizeof(Inner) + sv.size()
      );
      Inner *inner = reinterpret_cast<Inner *>(data);
      
      inner->ref_count = 1;
      inner->size = static_cast<uint32_t>(sv.size());
      std::copy(sv.begin(), sv.end(), inner->data);

      inner->data[sv.size()] = '\0';
    }
  }

  ~RcString() {
    if (data) {
      Inner *inner = reinterpret_cast<Inner *>(data);
      inner->ref_count--;
      if (inner->ref_count == 0) {
        std::allocator_traits<A>::deallocate(
            allocator, data, sizeof(Inner) + inner->size
        );
        //data = nullptr;
      }
    }
  }
};
} // namespace cinnabar
