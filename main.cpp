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
	template < int ...Args >
	static auto merge( integer< Args... > ) -> integer< Value, Values..., Args... >;

	// template < typename T >
	// using append = decltype( append_helper( std::declval< T >() ) );

	constexpr operator int() const
	{
		return decimal_value< Value, Values... >();
	}
	
	static std::ostream& print( std::ostream &s )
	{
		return s << "integer " << decimal_value< Value, Values... >();
	}
};

template < int Value >
struct lower_case
{
	static constexpr auto value = Value;
	static std::ostream& print( std::ostream &s )
	{
		return s << "lower case " << Value;
	}
};

template < int Value >
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

template < int C >
struct whitespace
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "whitespace(" << C << ')';
	}
};

template < int T >
struct error_type
{
	static std::ostream& print( std::ostream &s )
	{
		return s << "error_type(" << T << ')';
	}
};

template < int C >
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
	else if constexpr ( C == ' ' || C == '\t' || C == '\n' || C == '\r' )
	{
		return whitespace< C >{};
	}
	else
	{
		return error_type< C >{};
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

void print( std::ostream&, std::tuple<> ) {}

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
	static auto append( T ) -> array< Types..., T >;

	static std::ostream& print( std::ostream &s )
	{
		s << "array[ ";
		::print( s, tuple_type{} );
		s << " ]";
		return s;
	}
};

struct not_set {};

template < typename Key = not_set, typename Value = not_set >
struct key_value
{
	using key = Key;
	using value = Value;

	template < typename T >
	static auto assign_key(T) -> key_value< T, Value >;

	template < typename T >
	static auto assign_value( T ) -> key_value< Key, T >;

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

template< typename Test, template < int... > class Type >
struct is_templated_int_collection : std::false_type {};

template< template < int... > class Type, int ...Args >
struct is_templated_int_collection< Type< Args... >, Type > : std::true_type {};

template < typename T >
constexpr inline bool is_integer_v = is_templated_int_collection< T, integer >::value;

template < typename T >
constexpr inline bool is_whitespace_v = is_templated_int_collection< T, whitespace >::value;


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
		static_assert( sizeof...(Rest) == 0, "could not find Key in Key/Value storage" );
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

template < typename Lambda, int Index = 0 >
constexpr auto tokenize( Lambda str_lambda )
{
    constexpr auto str = str_lambda();
	if constexpr ( Index < str.size() - 1 )
	{
		return token_string_t<
			decltype( determine_token< str[Index] >() ),
			decltype( tokenize< Lambda, Index + 1 >( str_lambda ) )
		>{};
	}
	else
	{
		return token_string_t<
			decltype( determine_token< str[Index] >() )
		>{};
	}
}

template < typename ...Rest >
constexpr auto parse_string( Rest... );

template < typename ...Types >
struct object
{
	template < typename T >
	static auto append( T ) -> object< Types..., T >;

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

template < int C, int ...Rest >
void print_as_char( std::ostream &s )
{
	s << char( C );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		print_as_char< Rest... >( s );
	}
}

template < int ...Types >
struct string
{
	template < template < int... > class Character, int ...Args >
	static auto append( Character< Args... > ) -> string< Types..., Args... >;

	static std::ostream& print( std::ostream &s )
	{
		s << "string\"";
		print_as_char< Types... >( s );
		s << "\"";
		return s;
	}
};

template< typename T >
constexpr inline bool is_string_v = is_templated_int_collection< T, string >::value;

template< typename T >
constexpr inline bool is_object_v = is_specialization< T, object >::value;

template< typename T >
constexpr inline bool is_key_value_v = is_specialization< T, key_value >::value;

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

auto merge_integers( token_string<> ) -> token_string<>;

template < typename A >
auto merge_integers( token_string< A > ) -> token_string< A >;

template < typename A, typename B, typename ...Rest >
auto merge_integers( token_string< A, B, Rest... > )
{
	using all_but_a_type = token_string_t< B, Rest... >;
	using rest_type = decltype( merge_integers( all_but_a_type{} ) );
	using skip_a_and_continue = token_string_t< A, rest_type >;

	if constexpr ( is_integer_v< A > )
	{
		if constexpr ( is_integer_v< B > )
		{
			using merged = decltype( A::merge( B{} ) );
			return merge_integers( token_string_t< merged, Rest... >{} );
		}
		else
		{
			return skip_a_and_continue{};
		}
	}
	else
	{
		return skip_a_and_continue{};
	}
}

static_assert( 
	same_types<
		decltype( merge_integers( token_string_t< integer<1>, integer<2,3> >{} ) ),
		token_string_t< integer<1,2,3> >
	>
);

static_assert( integer<0,1,2,3>{} == 123 );

auto parse_array( token_string<> ) -> token_string<>;
template < typename T >
auto parse_array( token_string<T> ) -> token_string<T>;


template < typename A, typename B, typename ...Rest >
constexpr auto parse_array( token_string< A, B, Rest... > )
{
	if constexpr ( same_types< A, array_start > )
	{
		return parse_array( token_string_t< array<>, B, Rest... >{} );
	}
	else if constexpr ( is_specialization< A, array >::value )
	{
		if constexpr ( same_types< B, array_end > )
		{
			using remaining_type = decltype( parse_array( token_string_t< Rest... >{} ) );
			return token_string_t< A, remaining_type >{};
		}
		else
		{
			return parse_array( 
				token_string_t<
					decltype( A::append( B{} ) ),
					token_string< Rest... >
				>{}
			);
		}
	}
	else
	{
		using parse_all_but_a = decltype( parse_array( token_string_t< B, Rest... >{} ) );
		return token_string_t< A, parse_all_but_a >{};
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
	else if constexpr ( is_object_v< A > )
	{
		if constexpr ( is_key_value_v< B > )
		{
			return parse_object( 
				token_string_t< 
					decltype( A::append( B{} ) ),
					Rest... 
				>{}
			);
		}
		else if constexpr ( same_types< B, object_end > )
		{
			using parse_rest = decltype( parse_object( token_string_t< Rest... >{} ) );
			return token_string_t< A, parse_rest >{};
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
			static_assert( sizeof(token_string<A,B,Rest...>)<0, "expected comma, } or key/value" );
		}
	}
	else
	{
		using parse_all_but_a = decltype( parse_object( token_string_t< B, Rest... >{} ) );
		return token_string_t< A, parse_all_but_a >{};
	}
}

auto parse_key_value( token_string<> ) -> token_string<>;
template < typename A >
auto parse_key_value( token_string<A> ) -> token_string<A>;
template < typename A, typename B >
auto parse_key_value( token_string<A,B> ) -> token_string<A,B>;

template < typename A, typename B, typename C, typename ...Rest >
constexpr auto parse_key_value( token_string< A, B, C, Rest... > = {} )
{
	using all_but_a_type = token_string_t< B, C, Rest... >;
	using rest_type = decltype( parse_key_value( all_but_a_type{} ) );
	using skip_a_and_continue = token_string_t< A, rest_type >;

	if constexpr ( is_string_v< A > )
	{
		if constexpr ( same_types< B, colon > )
		{
			using parse_rest = decltype( parse_key_value( token_string_t< Rest...>{} ) );
			return token_string_t< key_value< A, C >, parse_rest >{};
		}
		else
		{
			return skip_a_and_continue{};
		}
	}
	else
	{
		return skip_a_and_continue{};
	}
}

static_assert(
	same_types<
		decltype( parse_key_value( token_string_t< string<1,2,3>, colon, integer<1,2,3> >{} ) ),
		token_string< key_value< string<1,2,3>, integer<1,2,3> > >
	>
);

auto parse_string( token_string<> ) -> token_string<>;
template < typename A >
auto parse_string( token_string<A> ) -> token_string<A>;

template < typename A, typename B, typename ...Rest >
constexpr auto parse_string( token_string< A, B, Rest... > = {} )
{
	if constexpr ( same_types< A, double_quote > )
	{
		return parse_string( token_string_t< string<>, B, Rest... >{} );
	}
	else if constexpr ( is_string_v< A > )
	{
		if constexpr ( same_types< B, double_quote > )
		{
			using rest_type = decltype( parse_string( token_string< Rest... >{} ) );
			return token_string_t< A, rest_type >{};
		}
		else
		{
			return parse_string(
				token_string_t<
					decltype( A::append( B{} ) ),
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

constexpr auto remove_whitespace( token_string<> ) -> token_string<>
{
	return {};
}

template < typename A, typename ...Rest >
constexpr auto remove_whitespace( token_string< A, Rest... > = {} )
{
	if constexpr ( is_whitespace_v< A > )
	{
		return remove_whitespace( token_string_t< Rest... >{} );
	}
	else
	{
		using rest_type = decltype( remove_whitespace( token_string_t< Rest... >{} ) );
		return token_string_t< A, rest_type >{};
	}
}

#define constexpr_string(...) ([]() constexpr -> std::string_view { return __VA_ARGS__; })

int main( int, char ** )
{
	const auto json_string = constexpr_string(
		R"json(
		{
			"aap":738,
			"noot":"mies",
			"kees":[0,1,4]
		}
		)json"
	);

	const auto tokens = tokenize( json_string );
	using object = decltype( tokens );

	// object::print( std::cout ) << '\n';

	using object_merged_integer = decltype( merge_integers( object{} ) );

	using object_merged_integer_string = decltype( parse_string( object_merged_integer{} ) );
	// object_merged_integer_string::print( std::cout ) << '\n';
	
	using no_whitespace = decltype( remove_whitespace( object_merged_integer_string{} ) );
	// no_whitespace::print( std::cout ) << '\n';
	
	using parse_array = decltype( parse_array( no_whitespace{} ) );

	using key_values = decltype( parse_key_value( parse_array{} ) );
	key_values::print( std::cout ) << '\n';

	using parsed = decltype( parse_object( key_values{} ) );
	// parsed::print( std::cout ) << '\n';

	using obj = decltype( get_nth_element< 0 >( parsed{} ) );

	using first_key_val = decltype( get_nth_element< 0 >( obj{} ) );

	const auto aap = parse_string( tokenize( constexpr_string( R"json("aap")json" ) ) );
	using aap_t = decltype( get_nth_element< 0 >( aap ) );

	aap_t::print( std::cout ) << std::endl;

	std::cout << 
	obj::get< aap_t >()
	 << std::endl;

	return first_key_val::value{};
}
