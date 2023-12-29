#pragma once

#include <new>
#include <utility>
#include <cassert>
#include <array>

template <typename T, typename _Counter>
struct sptr_block;

template <typename T, typename _Counter>
struct sptr_block_deleter_base
{
	constexpr sptr_block_deleter_base() = default;

	virtual ~sptr_block_deleter_base() {}
	virtual void destroy_value(T *ptr) = 0;
	virtual void destroy_block(sptr_block<T, _Counter> *ptr) = 0;
};

template <typename T, typename _Counter>
struct sptr_block
{
	T *val;
	sptr_block_deleter_base<T, _Counter> *deleter;
	_Counter strong;
	_Counter weak;
};

template <typename T, typename _Counter>
struct sptr_block_deleter_default : sptr_block_deleter_base<T, _Counter>
{
	constexpr sptr_block_deleter_default() = default;

	virtual void destroy_value(T *ptr) override
	{
		delete ptr;
	}

	virtual void destroy_block(sptr_block<T, _Counter> *ptr) override
	{
		delete ptr;
	}
};

template <typename T, typename _Counter>
static inline sptr_block_deleter_default<T, _Counter> _sptr_block_deleter_default_global = {};

template <typename T, typename _Counter>
struct sptr_block_deleter_bound : sptr_block_deleter_base<T, _Counter>
{
	virtual void destroy_value(T *ptr) override
	{
		// value, block, deleter share same memory
		ptr->~T();
	}

	virtual void destroy_block(sptr_block<T, _Counter> *ptr) override
	{
		// value, block, deleter share same memory
		delete ptr;
	}
};

template <typename T, typename _Counter>
static inline sptr_block_deleter_bound<T, _Counter> _sptr_block_deleter_bound_global = {};

template <typename T, typename _Counter>
struct sptr_block_deleter_empty : sptr_block_deleter_base<T, _Counter>
{
	virtual void destroy_value(T *ptr) override
	{
		ptr->~T();
	}

	virtual void destroy_block(sptr_block<T, _Counter> *ptr) override
	{
	}
};

template <typename T, typename _Counter>
static inline sptr_block_deleter_empty<T, _Counter> _sptr_block_deleter_empty_global = {};

template <typename T, typename _Counter>
struct sptr_block_bound
{
	sptr_block<T, _Counter> block = {};

	alignas(T)
	char buffer[sizeof(T)];

	constexpr T *buffer_value() noexcept { return (T *)buffer; }

	template <typename... TArgs>
	sptr_block_bound(TArgs&&... args)
	{
		new (buffer) T(std::forward<TArgs>(args)...);
	}
};

template <typename _T, typename _Counter = int>
struct sptr
{
	using value_type = _T;
	using counter_type = _Counter;
	using block_type = sptr_block<value_type, counter_type>;

	block_type *_block;

	constexpr void _incref() const noexcept
	{
		if (_block)
		{
			++_block->strong;
		}
	}

	constexpr void _decref() noexcept
	{
		if (_block)
		{
			if (--_block->strong == 0)
			{
				_block->deleter->destroy_value(_block->val);
				_block->val = nullptr;

				if (--_block->weak == 0)
				{
					_block->deleter->destroy_block(_block);
					_block = nullptr;
				}
			}
		}
	}

	constexpr sptr(const sptr &other)
	{
		other._incref();
		_block = other._block;
	}

	constexpr sptr(sptr &&other) noexcept
	{
		_block = other._block;
		other._block = nullptr;
	}

	template <typename _T2>
	constexpr sptr(const sptr<_T2, counter_type> &other) : sptr(other.casted<value_type>())
	{
	}
	
	template <typename _T2>
	constexpr sptr(sptr<_T2, counter_type> &&other) noexcept : sptr(static_cast<sptr<_T2, counter_type> &&>(other).casted<value_type>())
	{
	}

	constexpr sptr &operator = (const sptr &other)
	{
		sptr(other).swap(*this);
		return *this;
	}

	constexpr sptr &operator = (sptr &&other) noexcept
	{
		sptr(static_cast<sptr &&>(other)).swap(*this);
		return *this;
	}

	constexpr sptr() : _block{ nullptr }
	{
	}

	constexpr ~sptr() noexcept
	{
		_decref();
	}

	template <typename... TArgs>
	static constexpr sptr create_bounded(TArgs&&... args)
	{
		sptr result;

		auto tmp = new sptr_block_bound<value_type, counter_type>(std::forward<TArgs>(args)...);

		result._block = &tmp->block;
		result._block->val = tmp->buffer_value();
		result._block->deleter = &_sptr_block_deleter_bound_global<value_type, counter_type>;
		result._block->strong = 1;
		result._block->weak = 1;

		return result;
	}

	static constexpr sptr create_with_empty_deleter(value_type *val, sptr_block<value_type, counter_type> *in_place_ptr)
	{
		sptr result;

		result._block = new (in_place_ptr) sptr_block<value_type, counter_type>();
		result._block->val = val;
		result._block->deleter = &_sptr_block_deleter_empty_global<value_type, counter_type>;
		result._block->strong = 1;
		result._block->weak = 1;

		return result;
	}

	constexpr void swap(sptr &other) noexcept
	{
		std::swap(_block, other._block);
	}

	constexpr value_type *get() const noexcept { assert(_block); return _block->val; }
	constexpr value_type *operator -> () const noexcept { return get(); }
	constexpr value_type &operator * () const noexcept { return *get(); }

	constexpr explicit operator bool () const noexcept { return (bool)_block; }

	// UNSAFE AS F*CK!!!
	template <typename other_value_type>
	constexpr sptr<other_value_type, counter_type> &casted() & noexcept
	{
		assert((other_value_type *)(void *)123 == (value_type *)(void *)123);

		return *reinterpret_cast<sptr<other_value_type, counter_type> *>(this);
	}

	// UNSAFE AS F*CK!!!
	template <typename other_value_type>
	constexpr const sptr<other_value_type, counter_type> &casted() const & noexcept
	{
		assert((other_value_type *)(void *)123 == (value_type *)(void *)123);

		return *reinterpret_cast<const sptr<other_value_type, counter_type> *>(this);
	}

	// UNSAFE AS F*CK!!!
	template <typename other_value_type>
	constexpr sptr<other_value_type, counter_type> casted() && noexcept
	{
		assert((other_value_type *)(void *)123 == (value_type *)(void *)123);

		return static_cast<sptr<other_value_type, counter_type> &&>(*reinterpret_cast<sptr<other_value_type, counter_type> *>(this));
	}
};

template <typename _T, typename _Counter = int, size_t _BucketSize = 128>
struct sptr_memory_pool
{
	sptr_memory_pool() = default;

	sptr_memory_pool(const sptr_memory_pool &) = delete;
	sptr_memory_pool &operator = (const sptr_memory_pool &) = delete;
	sptr_memory_pool(sptr_memory_pool &&) = delete;
	sptr_memory_pool &operator = (sptr_memory_pool &&) = delete;

	~sptr_memory_pool() noexcept
	{
		assert(_buckets.size() == 0 || (_buckets.size() == 1 && _buckets[0]->last == 0));
	}

	struct _bucket;

	struct _sptr_block_deleter_pool : sptr_block_deleter_base<_T, _Counter>
	{
		sptr_memory_pool *pool = nullptr;
		_bucket *bucket = nullptr;

		virtual void destroy_value(_T *ptr) override
		{
			--bucket->used_values;
			ptr->~_T();
		}

		virtual void destroy_block(sptr_block<_T, _Counter> *ptr) override
		{
			if (--bucket->used_blocks == 0)
			{
				for (auto it = pool->_buckets.begin(); it != pool->_buckets.end(); ++it)
				{
					if (it->get() == bucket)
					{
						if (it + 1 == pool->_buckets.end())
						{
							assert(bucket->used_values == 0);
							assert(bucket->used_blocks == 0);

							bucket->last = 0;
						}
						else
						{
							pool->_buckets.erase(it);
						}

						break;
					}
				}
			}
		}

		_sptr_block_deleter_pool(_bucket *bucket_, sptr_memory_pool *pool_) : bucket{ bucket_ }, pool{ pool_ }
		{
		}
	};

	struct _sptr_block_pool
	{
		sptr_block<_T, _Counter> block = {};
		_T bounded_value;
		_sptr_block_deleter_pool deleter;

		template <typename... TArgs>
		_sptr_block_pool(_bucket *bucket_, sptr_memory_pool *pool_, TArgs&&... args) : bounded_value{ std::forward<TArgs>(args)... }, deleter{ bucket_, pool_ }
		{
		}
	};

	struct _bucket
	{
		struct _sptr_block_pool_unused
		{
			alignas(_sptr_block_pool)
			char buffer[sizeof(_sptr_block_pool)];

			constexpr _sptr_block_pool *buffer_value() noexcept { return (_sptr_block_pool *)buffer; }
		};

		size_t used_values = 0;
		size_t used_blocks = 0;
		size_t last = 0;
		std::array<_sptr_block_pool_unused, _BucketSize> arr;

		template <typename... TArgs>
		_sptr_block_pool *create(sptr_memory_pool *pool, TArgs&&... args)
		{
			assert(last != _BucketSize);

			_sptr_block_pool_unused &unused = arr[last];

			++used_values;
			++used_blocks;
			++last;

			return new (unused.buffer_value()) _sptr_block_pool(this, pool, std::forward<TArgs>(args)...);
		}
	};

	std::vector<std::unique_ptr<_bucket>> _buckets;

	template <typename... TArgs>
	constexpr sptr<_T, _Counter> create(TArgs&&... args)
	{
		sptr<_T, _Counter> result;

		if (_buckets.empty() || _buckets.back()->last == _BucketSize)
			_buckets.emplace_back(std::make_unique<_bucket>());

		_bucket *bucket = _buckets.back().get();

		_sptr_block_pool *tmp = bucket->create(this, std::forward<TArgs>(args)...);
		result._block = &tmp->block;
		result._block->val = &tmp->bounded_value;
		result._block->deleter = &tmp->deleter;
		result._block->strong = 1;
		result._block->weak = 1;

		return result;
	}
};

