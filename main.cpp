#include <cstddef>
#include <array>
#include <iostream>
#include <tuple>
#include <map>

#define constexpr_string(...) ([]() constexpr -> std::string_view { return __VA_ARGS__; })

struct array_start {};
struct array_end {};

struct object_start {};
struct object_end {};

template < int Value >
struct integer
{
	template < int A >
	static auto merge( integer< A > ) -> integer< Value * 10 + A >;

	constexpr operator int() const
	{
		return Value;
	}
};

template < int Value >
struct lower_case {};

template < int Value >
struct upper_case {};

struct equals {};

struct double_quote {};

struct colon {};

struct comma {};

template < int C >
struct whitespace {};

template < int T >
struct error_type {};

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

#if 0
constexpr auto determine_token2( char C )
{
	if ( C == '[' )
	{
		return array_start{};
	}
	else if ( C >= '0' && C <= '9' )
	{	
		return integer< C - '0' >{};
	}
	else if ( C == ']' )
	{
		return array_end{};
	}
	else if ( C == '{' )
	{
		return object_start{};
	}
	else if ( C == '}' )
	{
		return object_end{};
	}
	else if ( C >= 'a' && C <= 'z' )
	{
		return lower_case< C >{};
	}
	else if ( C >= 'A' && C <= 'Z' )
	{
		return upper_case< C >{};
	}
	else if ( C == '=' )
	{
		return equals{};
	}
	else if ( C == '"' )
	{
		return double_quote{};
	}
	else if ( C == ':' )
	{
		return colon{};
	}
	else if ( C == ',' )
	{
		return comma{};
	}
	else if ( C == ' ' || C == '\t' || C == '\n' || C == '\r' )
	{
		return whitespace< C >{};
	}
	else
	{
		return error_type< C >{};
	}
}
#endif

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

template < typename A, typename B >
constexpr inline bool same_types = std::is_same< std::remove_cv_t< A >, std::remove_cv_t< B > >::value;

template< typename Test, template< typename... > class Ref >
struct is_specialization : std::false_type {};

template< template< typename... > class Ref, typename ...Args >
struct is_specialization< Ref< Args... >, Ref > : std::true_type {};


template < template < typename... > typename Test, typename ...Args >
struct contains_specialization_type : std::false_type {};

template < template < typename... > typename Test, typename T, typename ...Args >
struct contains_specialization_type< Test, T, Args... > 
{
	static constexpr inline bool value = is_specialization< T, Test >::value || contains_specialization_type< Test, Args... >::value;
};

template < typename ...Types >
struct token_string
{
	using type = token_string;
	using tuple_type = std::tuple< Types... >;

	static constexpr auto append( token_string<> )
	{
		return token_string< Types... >{};
	}

	template < typename A >
	static constexpr auto append( A )
	{
		return token_string< Types..., A >{};
	}

	template < typename A >
	static constexpr auto append( token_string< A > )
	{
		return append( A{} );
	}

	template < typename A, typename B, typename ...Args >
	static constexpr auto append( token_string< A, B, Args... > )
	{
		using first_added = decltype( type::append( A{} ) );
		return first_added::append( token_string< B, Args... >{} );
	}

	static auto result()
	{
		return std::get< 0 >( tuple_type{} );
	}
};

auto make_token_string()
{
	return token_string<>{};
}

template < typename T, typename ...Rest >
auto make_token_string( T, Rest... )
{
	return token_string< T >::append( token_string< Rest... >{} );
}

template < typename ...Types >
using make_token_string_t = decltype( make_token_string( Types{}... ) );

template < typename ...Types >
using token_string_t = decltype( make_token_string( Types{}... ) );

template < typename Test, typename ...Args >
struct contains_type : std::false_type {};

template < typename Test, typename T, typename ...Args >
struct contains_type< Test, T, Args... > 
{
	static constexpr inline bool value = same_types< Test, T > || contains_type< Test, Args... >::value;
};

template < int N, typename Value, typename ...Rest >
auto lookup_nth_value()
{
	if constexpr ( N == 0 )
	{
		return Value{};
	}
	else
	{
		return lookup_nth_value< N - 1, Rest... >();
	}
}

template < typename ...Types >
struct array
{
	using tuple_type = std::tuple< Types... >;

	template < typename T >
	using append = array< Types..., T >;

	template < int N >
	auto operator[]( integer< N > )
	{
		return lookup_nth_value< N, Types... >();
	}
};

template < typename ...Types >
struct incomplete_array
{
	using end_node = array_end;

	template < typename T >
	using append = incomplete_array< Types..., T >;

	template < typename ...T >
	static auto from_token_string( token_string< T... > )
	{
		return array< T... >{};
	}

	using finalize = make_token_string_t< Types... >;
};

template < typename Key, typename Value >
struct key_value
{
	using key = Key;
	using value = Value;

	template < typename T >
	static auto assign_key(T) -> key_value< T, Value >;

	template < typename T >
	static auto assign_value( T ) -> key_value< Key, T >;
};


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
		static_assert( sizeof...(Rest) > 0, "could not find Key in Key/Value storage" );
		return lookup_key_value< Key, Rest... >();
	}
}

template < int Index, typename Token, typename Lambda >
constexpr auto find_token( Lambda lambda )
{
	constexpr auto str = lambda();
	if constexpr ( Index < str.size() )
	{
		if constexpr ( same_types< decltype( determine_token< str[ Index ] >() ), Token > )
		{
			return Index;
		}
		else
		{
			return find_token< Index + 1, Token >( lambda );
		}
	}
	else
	{
		return -1;
	}
};

template < int ...Types >
struct string
{
	template < typename T >
	static auto append(T )
	{
		static_assert( sizeof(T)!=sizeof(T) );
	}
	template < template < int... > class Character, int ...Args >
	static auto append( Character< Args... > ) -> string< Types..., Args... >;
};

template < int Start, int End, typename Lambda >
constexpr auto make_string( Lambda str_lambda )
{
    constexpr auto str = str_lambda();
	if constexpr ( Start < End )
	{
		return string< str[ Start ] >::append( make_string< Start + 1, End >( str_lambda ) );
	}
	else
	{
		return string<>{};
	}
}

template < int Start, int End, typename Lambda >
constexpr auto make_integer( Lambda str_lambda )
{
    constexpr auto str = str_lambda();
	if constexpr ( Start < End )
	{
		constexpr auto value = str[ Start ] - '0';
		return integer< value >::merge( make_integer< Start + 1, End >( str_lambda ) );
	}
	else
	{
		return integer< 0 >{};
	}
}

template< typename Test, template < int... > class Type >
struct is_templated_int_collection : std::false_type {};

template< template < int... > class Type, int ...Args >
struct is_templated_int_collection< Type< Args... >, Type > : std::true_type {};

template < typename T >
constexpr inline bool is_integer_v = is_templated_int_collection< T, integer >::value;

template < typename T >
constexpr inline bool is_whitespace_v = is_templated_int_collection< T, whitespace >::value;

template < int Index, typename Lambda >
constexpr auto find_first_non_integer( Lambda lambda )
{
	constexpr auto str = lambda();
	using type = decltype( determine_token< str[ Index ] >() );
	if constexpr ( !is_integer_v< type > )
	{
		return Index;
	}
	else
	{
		return find_first_non_integer< Index + 1 >( lambda );
	}
}


template < typename Lambda, int Index = 0 >
constexpr auto tokenize( Lambda str_lambda )
{
    constexpr auto str = str_lambda();
	if constexpr ( Index < str.size() )
	{
		using first = decltype( determine_token< str[Index] >() );

		if constexpr ( same_types< first, double_quote > )
		{
			constexpr int next_quote = find_token< Index + 1, double_quote >( str_lambda );
			if constexpr ( next_quote > Index )
			{
				using second = decltype( tokenize< Lambda, next_quote + 1 >( str_lambda ) );
				using string_type = decltype( make_string< Index + 1, next_quote >( str_lambda ) );
				return make_token_string( string_type{}, second{} );
			}
			else
			{
				static_assert( next_quote > Index, "quotes not balanced, check your JSON" );
			}
		}
		else if constexpr ( is_whitespace_v< first > )
		{
			return tokenize< Lambda, Index + 1 >( str_lambda );
		}
		else if constexpr ( is_integer_v< first > )
		{
			constexpr auto first_non_integer = find_first_non_integer< Index + 1 >( str_lambda );
			using integer_type = decltype( make_integer< Index, first_non_integer >( str_lambda ) );
			using second = decltype( tokenize< Lambda, first_non_integer >( str_lambda ) );
			return make_token_string( integer_type{}, second{} );
		}
		else
		{
			using second = decltype( tokenize< Lambda, Index + 1 >( str_lambda ) );
			return make_token_string( first{}, second{} );
		}
	}
	else
	{
		return token_string_t<>{};
	}
}

template < typename T >
auto parse( T );

template < typename ...Types >
struct object
{
	template < typename T >
	using append = object< Types..., T >;

	static constexpr bool size()
	{
		return sizeof...( Types );
	}

	template < typename T >
	auto operator[]( T key ) const
	{
		return get< decltype( parse( key ) ) >();
	}

	using tuple_type = std::tuple< Types... >;

	template < typename Key >
	static constexpr auto get()
	{
		return lookup_key_value< Key, Types... >();
	}
};

auto parse_key_value_list( token_string<> )
{
	return token_string<>{};	
}

template < typename A >
auto parse_key_value_list( token_string< A > )
{
	return token_string< A >{};
}

template < typename A, typename ...Rest >
auto parse_key_value_list( token_string< A, comma, Rest... > )
{
	return make_token_string( A{}, parse_key_value_list( Rest{}... ) );
}

auto remove_commas( token_string<> )
{
	return token_string<>{};
}

template < typename A  >
auto remove_commas( token_string< A > )
{
	return make_token_string( A{} );
}

template < typename A, typename B, typename ...Rest >
auto remove_commas( token_string< A, B, Rest... > )
{
	if constexpr ( same_types< B, comma > )
	{
		if constexpr ( sizeof...(Rest) > 0 )
		{
			return make_token_string(
				A{},
				remove_commas( make_token_string_t< Rest... >{} )
			);
		}
		else
		{
			return make_token_string( A{} );
		}
	}
	else
	{
		static_assert( sizeof(B), "expected comma separated list of values" );
	}
}

template < typename ...Types >
struct incomplete_object
{
	using end_node = object_end;
	template < typename T >
	using append = incomplete_object< Types..., T >;

	using finalize = decltype( make_token_string( Types{}... ) );

	template < typename ...T >
	static auto from_token_string( token_string< T... > )
	{
		return object< T... >{};
	}
};

template < typename T >
struct start_node_helper {};

template <>
struct start_node_helper< array_start >
{
	using type = incomplete_array<>;
};

template <>
struct start_node_helper< object_start >
{
	using type = incomplete_object<>;
};

template < typename T >
using start_node = typename start_node_helper< T >::type;


template < int C, int ...Rest >
void print_as_char( std::ostream &s )
{
	s << char( C );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		print_as_char< Rest... >( s );
	}
}

template< typename T >
constexpr inline bool is_string_v = is_templated_int_collection< T, string >::value;

template< typename T >
constexpr inline bool is_object_v = is_specialization< T, object >::value;

template< typename T >
constexpr inline bool is_incomplete_object_v = is_specialization< T, incomplete_object >::value;

template< typename T >
constexpr inline bool is_key_value_v = is_specialization< T, key_value >::value;

template < typename T >
constexpr inline bool is_token_string_v = is_specialization< T, token_string >::value;

template< typename T >
constexpr inline bool is_array_v = is_specialization< T, array >::value;

template< typename T >
constexpr inline bool is_incomplete_array_v = is_specialization< T, incomplete_array >::value;


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

auto parse_key_value( token_string<> ) -> token_string<>;
template < typename A >
auto parse_key_value( token_string<A> ) -> token_string<A>;
template < typename A, typename B >
auto parse_key_value( token_string<A,B> ) -> token_string<A,B>;

template < typename A, typename B, typename C, typename ...Rest >
constexpr auto parse_key_value( token_string< A, B, C, Rest... > = {} )
{
	using all_but_a_type = make_token_string_t< B, C, Rest... >;
	using rest_type = decltype( parse_key_value( all_but_a_type{} ) );
	using skip_a_and_continue = make_token_string_t< A, rest_type >;

	if constexpr ( is_string_v< A > && same_types< B, colon > )
	{
		using parse_rest = decltype( parse_key_value( make_token_string( Rest{}... ) ) );
		return make_token_string_t< key_value< A, C >, parse_rest >{};
	}
	else
	{
		return skip_a_and_continue{};
	}
}

template < typename ...Types >
using parse_key_value_t = decltype( parse_key_value( Types{}... ) );

auto parse_array_object( token_string<> ) -> token_string<>;
template < typename A >
auto parse_array_object( token_string<A> ) -> decltype( make_token_string( A{} ) );

template < typename A, typename B, typename ...Rest >
constexpr auto parse_array_object( token_string< A, B, Rest... > )
{
	using all_but_a = make_token_string_t< B, Rest... >;
	using parse_all_but_a = decltype( parse_array_object( all_but_a{} ) );

	if constexpr ( same_types< A, object_start > || same_types< A, array_start > )
	{
		return parse_array_object( 
			make_token_string( 
				start_node< A >{}, 
				parse_all_but_a{} 
			)
		);
	}
	else if constexpr ( is_incomplete_object_v< A > || is_incomplete_array_v< A > )
	{
		using rest = make_token_string_t< Rest... >;
		using parse_rest = decltype( parse_array_object( rest{} ) );

		if constexpr ( same_types< B, typename A::end_node > )
		{
			return make_token_string(
				A::from_token_string(
					remove_commas(
						parse_key_value( 
							parse_array_object(
								typename A::finalize{} 
							)
						)
					)
				),
				parse_array_object( parse_rest{} )
			);
		}
		else
		{
			return parse_array_object( 
				make_token_string(
					typename A::template append< B >{},
					parse_rest{}
				)
			);
		}
	}
	else
	{
		return make_token_string( A{}, parse_all_but_a{} );
	}
}

template < typename T >
void print( T )
{
	std::cout << typeid(T).name();
}

template < typename Delimiter, typename T, typename ...Rest >
void print_delimited( Delimiter delimit, T t, Rest...rest )
{
	print( t );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		std::cout << delimit;
		print_delimited( delimit, rest... );
	}
}

template < typename ...Args >
void print( token_string< Args... > )
{
	std::cout << "token_string<";
	if constexpr ( sizeof...( Args ) > 0 )
	{
		print_delimited( ", ", Args{}... );
	}
	std::cout << ">";
}

template < typename Key, typename Value >
void print( key_value< Key, Value > )
{
	print( Key{} );
	std::cout << ':';
	print( Value{} );
}

template < int C, int ...Rest >
void print_as_characters()
{
	std::cout << char( C );
	if constexpr ( sizeof...( Rest ) > 0 )
	{
		print_as_characters< Rest... >();
	}
}

template < int ...Args >
void print( string< Args... > )
{
	std::cout << '"';
	print_as_characters< Args... >();
	std::cout << '"';
}

template < int ...Args >
void print( integer< Args... > )
{
	std::cout << integer< Args... >{};
}

template < typename ...Args >
void print( object< Args... > )
{
	std::cout << "{";
	if constexpr ( sizeof...( Args ) > 0 )
	{
		print_delimited( ',', Args{}... );
	}
	std::cout << '}';
}

template < typename ...Args >
void print( array< Args... > )
{
	std::cout << '[';
	if constexpr ( sizeof...( Args ) > 0 )
	{
		print_delimited( ", ", Args{}... );
	}
	std::cout << ']';
}

template < typename T >
auto parse( T )
{
	using parsed_complete = decltype( parse_array_object( T{} ) );
	return parsed_complete::result();
}

int main( int, char ** )
{
	const auto json_string = constexpr_string(
		R"json(
		{
			"aap":738,
			"noot":"mies",
			"kees":[0,1,4],
			"gnoe":[[[1]]],
			"das":{
				"zeker":"wel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijdenwel dit is een lange string en leidt tot lange compile tijden",
				"a":{
					"b":{
						"c":131237126
					}
				},
				"mooi":10
			}
		}
		)json"
	);

	const auto tokens = tokenize( json_string );
	// print( tokens );
	auto parsed = parse( tokens );
	print( parsed );
	// std::cout << parsed[ tokenize( constexpr_string( "\"noot\"" ) ) ] << '\n';

	// using obj = decltype( get_nth_element< 0 >( parsed ) );

	// using first_key_val = decltype( get_nth_element< 0 >( obj{} ) );

	// print( first_key_val{} );
	
	const auto aap = parse( tokenize( constexpr_string( R"json( "kees" )json" ) ) );
	print( aap );
	std::cout <<'\n';

	// const auto index_2 = parse( tokenize( constexpr_string( R"json( 2 )json" ) ) );

	// std::cout << parsed.get< decltype( aap ) >()[ index_2 ] << " \n";
}
