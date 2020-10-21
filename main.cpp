#include <cstddef>
#include <array>
#include <iostream>
#include <tuple>
#include <map>

struct array_start
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "array_start";
	}
};
struct array_end
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "array_end";
	}
};

struct object_start
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "object_start";
	}
};
struct object_end
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "object_end";
	}
};

template < size_t PowerOfTen >
constexpr int power_of_ten()
{
	if constexpr ( PowerOfTen > 0 )
	{
		return 10 * power_of_ten< PowerOfTen - 1 >();
	}
	else
	{
		return 1;
	}
}

template < int Value, int ...Values >
constexpr auto decimal_value()
{
	if constexpr ( sizeof...( Values ) > 0 )
	{
		return Value * power_of_ten< sizeof...( Values )  >() + decimal_value< Values... >();
	}
	else
	{
		return Value;
	}
}

template < int Value, int ...Values >
void debug_values( std::ostream& s )
{
	s << Value;
	if constexpr ( sizeof...(Values) > 0 )
	{
		debug_values< Values... >( s );
	}
}

template < int Value, int ...Values >
struct integer
{
	constexpr operator int() const
	{
		return decimal_value< Value, Values... >();
	}
	
	static std::ostream& print( std::ostream &s )
	{
		return s << "integer " << decimal_value< Value, Values... >();
	}
};

template < char Value >
struct lower_case
{
	static constexpr auto value = Value;
	static std::ostream& print( std::ostream &s )
	{
		return s << "lower case " << Value;
	}
};

template < char Value >
struct upper_case
{
	static constexpr auto value = Value;
	static std::ostream& print( std::ostream &s )
	{
		return s << "upper case " << Value;
	}
};

struct equals
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "equals";
	}
};

struct double_quote
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "double_quote";
	}
};

struct colon
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "colon";
	}
};

struct comma
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "comma";
	}
};

struct error_type
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "error_type";
	}
};

template < char C >
constexpr auto determine_token()
{
	if constexpr ( C == '[' )
	{
		return array_start{};
	}
	else if constexpr ( C >= '0' && C <= '9' )
	{	
		return integer< C - '0' >{};
	}
	else if constexpr ( C == ']' )
	{
		return array_end{};
	}
	else if constexpr ( C == '{' )
	{
		return object_start{};
	}
	else if constexpr ( C == '}' )
	{
		return object_end{};
	}
	else if constexpr ( C >= 'a' && C <= 'z' )
	{
		return lower_case< C >{};
	}
	else if constexpr ( C >= 'A' && C <= 'Z' )
	{
		return upper_case< C >{};
	}
	else if constexpr ( C == '=' )
	{
		return equals{};
	}
	else if constexpr ( C == '"' )
	{
		return double_quote{};
	}
	else if constexpr ( C == ':' )
	{
		return colon{};
	}
	else if constexpr ( C == ',' )
	{
		return comma{};
	}
	else
	{
		return error_type{};
	}
}

template < typename T >
struct is_followed_by
{
	static constexpr auto value = 0;
};

template < typename T, typename ...Rest >
void print_children( std::ostream &s, std::tuple< T, Rest... > )
{
	T::print( s );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		s << ", ";
		print_children( s, std::tuple< Rest... >{} );
	}
}

void print( std::ostream &s, std::tuple<> ) {}

template < typename T, typename ...Rest >
void print( std::ostream &s, std::tuple< T, Rest... > )
{
	T::print( s );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		s << ", ";
		print_children( s, std::tuple< Rest... >{} );
	}
}

template < typename ...Types >
struct array
{
	using tuple_type = std::tuple< Types... >;

	template < typename T >
	using append = array< Types..., T >;

	static std::ostream& print( std::ostream &s )
	{
		s << "array[ ";
		::print( s, tuple_type{} );
		s << " ]";
		return s;
	}
};


template < typename ...Types >
struct key
{
};

template < typename ...Types >
struct value
{
};


struct not_set {};

template < typename Key = not_set, typename Value = not_set, typename ...Types >
struct key_value
{
	using key = Key;
	using value = Value;

	template < typename T >
	using assign_key = key_value< T, Value, Types... >;

	template < typename T >
	using assign_value = key_value< Key, T, Types... >;

	static std::ostream& print( std::ostream &s )
	{
		s << "key_value{ ";
		::print( s, std::tuple< key >{} );
		s << ": ";
		::print( s, std::tuple< value >{} );
		s << " }";
		return s;
	}
};

template< typename Test, template< typename... > class Ref >
struct is_specialization : std::false_type {};

template< template< typename... > class Ref, typename ...Args >
struct is_specialization< Ref< Args... >, Ref > : std::true_type {};

template < typename A, typename B >
constexpr inline bool same_types = std::is_same< A, B >::value;

template < typename ...Types >
struct token_string;

template < typename Key, typename KeyValue, typename ...Rest >
auto lookup_key_value()
{
	if constexpr ( same_types< Key, typename KeyValue::key > )
	{
		return typename KeyValue::value{};
	}
	else
	{
		return lookup_key_value< Key, Rest... >();
	}
}

template < typename ...Types >
struct token_string
{
	using type = token_string;
	using tuple_type = std::tuple< Types... >;

	static std::ostream& print( std::ostream &s )
	{
		s << "token_string< ";
		::print( s, tuple_type{} );
		s << " >";
		return s;
	}
};

template < typename ...Types >
using token_string_t = typename token_string< Types... >::type;

template < char C, char ...Characters >
struct tokenize
{
	using first_token = decltype( determine_token< C >() );
	static auto type()
	{
		if constexpr ( sizeof...( Characters ) > 0 )
		{
			using second_token = decltype( tokenize< Characters... >::type() );
			return token_string_t< first_token, second_token >{};
		}
		else
		{
			return token_string_t< first_token >{};
		}
	}
};

template < typename ...Rest >
constexpr auto parse_string( Rest... );

template < typename ...Types >
struct object
{
	template < typename T >
	using append = object< Types..., T >;

	using tuple_type = std::tuple< Types... >;

	// using comma_found = 

	template < typename Key >
	static constexpr auto get()
	{
		return lookup_key_value< Key, Types... >();
	}

	static std::ostream& print( std::ostream &s )
	{
		s << "object{ ";
		::print( s, tuple_type{} );
		s << " }";
		return s;
	}
};

template < typename ...Types >
struct string
{
	template < typename T >
	using append = string< Types..., T >;

	static std::ostream& print( std::ostream &s )
	{
		s << "string\"";
		::print( s, std::tuple< Types... >{} );
		s << "\"";
		return s;
	}
};

namespace helper {
	template <class T1, class ...T>
	struct first
	{
		typedef T1 type;
	};

	template <class T1, class ...T>
	struct last
	{
		typedef typename last<T...>::type type;
	};

	template <class T1>
	struct last<T1>
	{
		typedef T1 type;
	};

	template < typename T, typename ...Args >
	inline constexpr bool first_equals_v = std::is_same< typename first< Args... >::type, T >::value;

	template < typename T, typename ...Args >
	inline constexpr bool last_equals_v = std::is_same< typename last< Args... >::type, T >::value;

	template < size_t N, typename T, typename ...Args >
	constexpr auto get_nth_element()
	{
		if constexpr ( N == 0 )
		{
			return std::declval< T >();
		}
		else
		{
			return get_nth_element< N - 1, Args... >();
		}
	}
}

template < size_t N, typename ...Args >
auto get_nth_element( Args... )
{
	return helper::get_nth_element< N, Args... >();
}

template < size_t N, template < typename ...T > class Template, typename ...Args >
auto get_nth_element( Template< Args... > )
{
	return helper::get_nth_element< N, Args... >();
}

// unpack nested token_string
template < typename T, typename ...Types, typename ...Rest >
struct token_string< T, token_string< Types... >, Rest... >
{
	using type = token_string_t< T, Types..., Rest... >;
};
template < typename ...Types, typename ...Rest >
struct token_string< token_string< Types... >, Rest... >
{
	using type = token_string_t< Types..., Rest... >;
};



template < typename Start, typename End, typename ...T >
struct specific_start_end_type
{
	using first = typename helper::first< T... >::type;
	using last = typename helper::last< T... >::type;

	static constexpr bool value = std::is_same< first, array_start >::value &&
		std::is_same< last, array_end >::value;
};

template < typename ...T >
using is_array = specific_start_end_type< array_start, array_end, T... >;

template < typename ...T >
using is_object = specific_start_end_type< object_start, object_end, T... >;

template < typename ...T >
auto parse( token_string< T... > )
{
	if constexpr ( is_array< T... >::value )
	{
		
	}
	else if constexpr ( is_object< T... >::value )
	{
		static_assert( sizeof...(T) == 0 );
	}
}

// decltype( tokenize<
// #include "test.json.array"
// >::type() ) kakjes;

template < typename A, typename B >
struct merged : token_string< A, B > {};

template < int ...A, int ...B >
struct merged< integer< A... >, integer< B... > >
{
	using type = token_string_t< integer< A..., B... > >;
};

template < typename A, typename B, typename ...Rest >
auto merge_integers( token_string< A, B, Rest... > )
{
	using start = token_string< A, B >;
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		using merged = typename merged< A, B >::type;
		if constexpr ( std::is_same< start, merged >::value )
		{
			return token_string_t< A, decltype( merge_integers( token_string_t< B, Rest... >{} ) ) >{};
		}
		else
		{
			return merge_integers( token_string_t< merged, Rest... >{} );
		}
	}
	else
	{
		return start{};
	}
}

// empty case
auto parse_array( token_string<> ) -> token_string<>;


template < typename A, typename B, typename ...Rest >
constexpr auto parse_array( token_string< A, B, Rest... > = {} )
{
	if constexpr ( same_types< A, array_start > )
	{
		return parse_array( token_string_t< array<>, B, Rest... >{} );
	}
	else if constexpr ( is_specialization< A, array >::value )
	{
		if constexpr ( same_types< B, array_end > )
		{
			return token_string_t<
				A,
				decltype( parse_array( token_string< Rest... >{} ) )
			>{};
		}
		else
		{
			return parse_array( 
				token_string_t<
					typename A::template append< B >,
					token_string< Rest... >
				>{}
			);
		}
	}
	else
	{
		return token_string_t< A, decltype( parse_array( token_string_t< B, Rest... >{} ) ) >{};
	}	
}

auto parse_object( token_string<> ) -> token_string<>;
template < typename A >
auto parse_object( token_string<A> ) -> token_string<A>;

template < typename A, typename B, typename ...Rest >
constexpr auto parse_object( token_string< A, B, Rest... > = {} )
{
	if constexpr ( same_types< A, object_start > )
	{
		return parse_object( 
			token_string_t< 
				object<>,
				B,
				Rest... 
			>{}
		);
	}
	else if constexpr ( is_specialization< A, object >::value )
	{
		if constexpr ( is_specialization< B, key_value >::value )
		{
			return parse_object( 
				token_string_t< 
					typename A::template append< B >,
					Rest... 
				>{}
			);
		}
		else if constexpr ( same_types< B, object_end > )
		{
			return token_string_t<
				A,
				decltype( parse_object( token_string_t< Rest... >{} ) )
			>{};
		}
		else if constexpr ( same_types< B, comma > )
		{
			return parse_object( 
				token_string_t< 
					A,
					Rest... 
				>{}
			);
		}
		else
		{
			static_assert( sizeof(B)<0, "expected comma, } or key/value" );
		}
	}
	else
	{
		return token_string_t< 
			A, 
			decltype(
				parse_object(
					token_string_t<
						B,
						Rest...
					>{}
				)
			)
		>{};
	}
}

// empty case
auto parse_key_value( token_string<> ) -> token_string<>;
// single case
template < typename A >
auto parse_key_value( token_string<A> ) -> token_string<A>;

template < typename A, typename B, typename ...Rest >
constexpr auto parse_key_value( token_string< A, B, Rest... > = {} )
{
	if constexpr ( is_specialization< A, string >::value )
	{
		if constexpr ( same_types< B, colon > )
		{
			return parse_key_value( token_string_t< key_value< A >, Rest... >{} );
		}
	}
	else if constexpr ( is_specialization< A, key_value >::value )
	{
		return token_string_t< 
			typename A::template assign_value< B >,
			decltype( parse_key_value( token_string_t< Rest... >{} ) )
		>{};
	}
	else
	{
		return token_string_t< 
			A, 
			decltype(
				parse_key_value(
					token_string_t<
						B,
						Rest...
					>{}
				)
			)
		>{};
	}
}

// empty case
auto parse_string( token_string<> ) -> token_string<>;
// single case
template < typename A >
auto parse_string( token_string<A> ) -> token_string<A>;

template < typename A, typename B, typename ...Rest >
constexpr auto parse_string( token_string< A, B, Rest... > = {} )
{
	if constexpr ( same_types< A, double_quote > )
	{
		return parse_string( token_string_t< string<>, B, Rest... >{} );
	}
	else if constexpr ( is_specialization< A, string >::value )
	{
		if constexpr ( same_types< B, double_quote > )
		{
			return token_string_t< 
				A,
				decltype( parse_string( token_string< Rest... >{} ) )
			>{};
		}
		else
		{
			return parse_string(
				token_string_t<
					typename A::template append< B >,
					Rest...
				>{}
			);
		}
	}
	else
	{
		return token_string_t< 
			A, 
			decltype(
				parse_string( token_string_t< B, Rest... >{} )
			)
		>{};
	}
}

int main( int argc, char **argv )
{
	using array = decltype( tokenize<'[', '8', ',', '4', ',', '2', ',', '7', '3', '8', ',', '0', ']'>::type() );
	// array::print( std::cout ) << '\n';

	using array_merged_integer = decltype( merge_integers( array{} ) );
	
	// parse_array( array_merged_integer{} ).print( std::cout ) << '\n';

	using object = decltype( 
		tokenize<
			'{',  
				'"', 'a', 'a', 'p', '"', ':', '7', '3', '8', 
				',', 
				'"', 'n', 'o', 'o', 't', '"', ':', '"', 'm', 'i', 'e', 's', '"', 
			'}'
		>::type()
	);
	object::print( std::cout ) << '\n';

	using object_merged_integer = decltype( merge_integers( object{} ) );

	using object_merged_integer_string = decltype( parse_string( object_merged_integer{} ) );
	// object_merged_integer_string::print( std::cout ) << '\n';

	using key_values = decltype( parse_key_value( object_merged_integer_string{} ) );
	// key_values::print( std::cout ) << '\n';

	using parsed = decltype( parse_object( key_values{} ) );
	parsed::print( std::cout ) << '\n';

	using obj = decltype( get_nth_element< 0 >( parsed{} ) );

	using first_key_val = decltype( get_nth_element< 0 >( obj{} ) );

	using aapt = decltype( parse_string( tokenize< '"', 'a', 'a', 'p', '"' >::type() ) );
	using aap = decltype( get_nth_element< 0 >( aapt{} ) );

	std::cout << 
	obj::get< aap >()
	 << std::endl;

	return first_key_val::value{};
}
