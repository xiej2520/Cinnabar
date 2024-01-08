#pragma once

#define FMT_HEADER_ONLY
#include "fmt/core.h"

#include <cstdint>

namespace cinnabar {

using std::size_t;

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>; // helloooo clang???

}
