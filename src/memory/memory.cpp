#include <memory_resource>
#include <vector>

void buffer_test() {
  std::pmr::monotonic_buffer_resource mbr{};
  std::pmr::polymorphic_allocator<int> bump_alloc{&mbr};
  std::pmr::vector<int> vec{bump_alloc};
}
