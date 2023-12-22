#pragma once

#include <vector>
#include <utility>

template <typename T, size_t N>
struct allocator_with_buffer
{
	using value_type = T;

	template <class U>
	struct rebind { typedef allocator_with_buffer<U, N> other; };

	allocator_with_buffer() = default;

	explicit allocator_with_buffer(T *buffer) : _buffer{ buffer } {}

	template <class U>
	allocator_with_buffer(const allocator_with_buffer<U, N> &other) : _buffer{ (T *)other._buffer } {}

	T* _buffer;

	constexpr void deallocate(T *const _Ptr, const size_t _Count) {
#if _ITERATOR_DEBUG_LEVEL != 0
		if constexpr (std::is_same_v<T, std::_Container_proxy>)
		{
			return;
		}
#endif // _ITERATOR_DEBUG_LEVEL == 0

		if (_Ptr == _buffer)
			return;

		std::allocator<T>{}.deallocate(_Ptr, _Count);
	}

	[[nodiscard]] constexpr __declspec(allocator) T *allocate(const size_t _Count) {
#if _ITERATOR_DEBUG_LEVEL != 0
		if constexpr (std::is_same_v<T, std::_Container_proxy>)
		{
			return (T *)_buffer - 1; // _buffer_Container_proxy
		}
#endif // _ITERATOR_DEBUG_LEVEL == 0

		if (_Count <= N)
			return _buffer;

		return std::allocator<T>{}.allocate(_Count);
	}
};

template <typename T, size_t N>
struct small_vector : std::vector<T, allocator_with_buffer<T, N>>
{
#if _ITERATOR_DEBUG_LEVEL != 0
	alignas(std::_Container_proxy)
	char _buffer_Container_proxy[sizeof(std::_Container_proxy)];
#endif // _ITERATOR_DEBUG_LEVEL == 0

	alignas(T)
	char _buffer[N * sizeof(T)];

	using _base = std::vector<T, allocator_with_buffer<T, N>>;

	small_vector() : _base(allocator_with_buffer<T, N>((T *)_buffer))
	{
		_base::reserve(N);
	}

	small_vector(const small_vector &other) : small_vector()
	{
		std::copy(other.begin(), other.end(), std::back_inserter(*this));
	}

	small_vector &operator = (const small_vector &other)
	{
		_base::clear();
		std::copy(other.begin(), other.end(), std::back_inserter(*this));
		return *this;
	}

	small_vector(small_vector &&other) : small_vector()
	{
		std::move(other.begin(), other.end(), std::back_inserter(*this));
	}

	small_vector &operator = (small_vector &&other)
	{
		_base::clear();
		std::move(other.begin(), other.end(), std::back_inserter(*this));
		return *this;
	}
};
