#include <cstddef>
#include <string_view>
#include <type_traits>
#include <array>
#include <cstdio>
#include <variant>

#define constexpr_string(...) ([]() constexpr -> std::string_view { return __VA_ARGS__; })

struct array_start {};
struct array_end {};

struct object_start {};
struct object_end {};

struct non_integer{};

template < int Value, int Length = 0 >
constexpr auto number_of_digits()
{
	if constexpr ( Value >= 10 )
	{
		return 1 + number_of_digits< Value / 10, Length + 1 >();
	}
	else
	{
		return 1;
	}
};

template < int Value, int Index = 0, typename Array >
constexpr void integer_to_array( Array &array )
{
	const auto last = array.size() - 1;
	if constexpr ( Value >= 10 )
	{
		integer_to_array< Value / 10, Index + 1 >( array );
		array[ last - Index ] = '0' + Value % 10;
	}
	else
	{
		array[ last- Index ] = '0' + Value;
	}
}

template < int ...Types >
struct string;

template < int Index = 0, int A, int ...Rest, typename Array >
constexpr void fill_array( string< A, Rest... >, Array &array )
{
	array[ Index ] = A;
	if constexpr ( sizeof...(Rest) > 0 )
	{
		fill_array< Index + 1 >( string< Rest... >{}, array );
	}
}

template < size_t A, size_t B >
constexpr auto add( std::array< char, A > a, std::array< char, B > b )
{
	std::array< char, A + B > result;
	auto dst = result.data();
	for ( auto c : a ) *dst++ = c;
	for ( auto c : b ) *dst++ = c;
	return result;
}

template < size_t A, size_t B, typename ...Rest >
constexpr auto add( std::array< char, A > a, std::array< char, B > b, Rest ...rest )
{
	return add( add( a, b ), rest... );
}

template < int ...Types >
struct string
{
	template < typename T >
	static auto append( T )
	{
		static_assert( sizeof(T)!=sizeof(T) );
	}
	template < template < int... > class Character, int ...Args >
	static auto append( Character< Args... > ) -> string< Types..., Args... >;

	template < typename T, typename ...Rest >
	static auto append( T, Rest... ) -> decltype( append( T{} ).append( Rest{}... ) );

	static constexpr auto to_string()
	{
		return string< '"' >::append( string{} ).append( string< '"' >{} );
	}

	static const std::array< char, sizeof...( Types ) >& characters()
	{
		static const auto result = []
		{
			std::array< char, sizeof...( Types ) > result;
			fill_array( string< Types... >{}, result );
			return result;
		}();
		return result;
	}
	
	static constexpr auto to_stringview()
	{
		return std::string_view( characters().data(), characters().size() );
	}
};

template < int Value >
constexpr auto integer_to_string()
{
	using result = string< Value % 10 + '0' >;
	if constexpr ( Value >= 10 )
	{
		using more_significand_digit = decltype( integer_to_string< Value / 10 >() );
		return decltype( more_significand_digit::append( result{} ) ){};
	}
	else
	{
		return string< Value + '0' >{};
	}
}


template < typename A, typename B >
constexpr inline bool same_types = std::is_same< std::remove_cv_t< A >, std::remove_cv_t< B > >::value;

template < int Value >
struct integer
{
	template < int A >
	static auto merge( integer< A > ) -> integer< Value * 10 + A >;

	static auto merge( non_integer ) -> integer< Value >;

	constexpr operator int() const
	{
		return Value;
	}

	static constexpr auto to_string()
	{
		return decltype( integer_to_string< Value >() ){};
	}
};

template < int Value >
struct lower_case {};

template < int Value >
struct upper_case {};

template < int Value >
struct special_character {};

struct double_quote {};

struct single_quote {};

struct colon {};

struct comma {};

template < int C >
struct whitespace {};

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
	else if constexpr ( C == '"' )
	{
		return double_quote{};
	}
	else if constexpr ( C == '\'' )
	{
		return single_quote{};
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
		return special_character< C >{};
	}
}

template< typename Test, template< typename... > class Ref >
struct is_specialization : std::false_type {};

template< template< typename... > class Ref, typename ...Args >
struct is_specialization< Ref< Args... >, Ref > : std::true_type {};


template < typename ...Types >
struct token_string
{
	using type = token_string;

	static constexpr auto append( token_string<> ) -> token_string< Types... >;

	template < typename A >
	static constexpr auto append( A ) -> token_string< Types..., A >;

	template < typename A >
	static constexpr auto append( token_string< A > )
	{
		return decltype( append( A{} ) ){};
	}

	template < typename A, typename B, typename ...Args >
	static constexpr auto append( token_string< A, B, Args... > )
	{
		using first_added = decltype( type::append( A{} ) );
		return decltype( first_added::append( token_string< B, Args... >{} ) ){};
	}
};

auto make_token_string() -> token_string<>;

template < typename T, typename ...Rest >
auto make_token_string( T, Rest... ) -> decltype( token_string< T >::append( token_string< Rest... >{} ) );

template < typename ...Types >
using make_token_string_t = decltype( make_token_string( Types{}... ) );

template < typename ...Types >
using token_string_t = decltype( make_token_string( Types{}... ) );

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

void insert_comma();

template < typename A >
auto insert_comma( A ) -> A;

template < int ...A, int ...B, typename ...Args >
auto insert_comma( string< A... >, string< B... >, Args... )
{
	if constexpr ( sizeof...( Args ) > 0 )
	{
		return string< A..., ',' >::append( 
			decltype( insert_comma( string< B... >{}, Args{}... ) ){}
		);
	}
	else
	{
		return string< A..., ',', B... >{};
	}
}

template < typename ...Types >
struct array
{
	template < typename T >
	using append = array< Types..., T >;

	template < int N >
	auto operator[]( integer< N > )
	{
		return lookup_nth_value< N, Types... >();
	}

	static constexpr auto to_string()
	{
		if constexpr ( sizeof...(Types) > 0 )
		{
			return string< '[' >::append( 
				insert_comma( Types::to_string()... ) 
			).append( 
				string< ']' >{}
			);
		}
		else
		{
			return string< '[', ']' >{};
		}
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

	static constexpr auto to_string()
	{
		return decltype(
			key::to_string().append( string< ':' >{} ).append( value::to_string() )
		){};
	}
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
}

template < int Start, int End, typename Lambda >
constexpr auto make_string( Lambda str_lambda )
{
	constexpr auto str = str_lambda();
	if constexpr ( Start < End )
	{
		return decltype(
			string< str[ Start ] >::append( make_string< Start + 1, End >( str_lambda ) )
		){};
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
		return non_integer{};
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

template < typename Lambda, size_t Index = 0 >
constexpr auto tokenize( Lambda str_lambda )
{
	constexpr auto str = str_lambda();
	if constexpr ( Index < str.size() )
	{
		using first = decltype( determine_token< str[Index] >() );

		if constexpr ( same_types< first, double_quote > || same_types< first, single_quote > )
		{
			constexpr int next_quote = find_token< Index + 1, first >( str_lambda );
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

	template < int ...Characters >
	auto operator[]( string< Characters... > ) const
	{
		return lookup_key_value< string< Characters... >, Types... >();
	}

	static constexpr auto to_string()
	{
		// using middle = string<  >;
		return decltype(
			string< '{' >::append( 
				insert_comma(
					Types::to_string()...
				)
			).append( string< '}' >{} )
		){};
	}
};

auto parse_key_value_list( token_string<> ) -> token_string<>;

template < typename A >
auto parse_key_value_list( token_string< A > ) -> token_string< A >;

template < typename A, typename ...Rest >
auto parse_key_value_list( token_string< A, comma, Rest... > ) -> 
	make_token_string_t< A, decltype( parse_key_value_list( Rest{}... ) ) >;

auto remove_commas( token_string<> ) -> token_string<>;

template < typename A  >
auto remove_commas( token_string< A > ) -> make_token_string_t< A >;

template < typename A, typename B, typename ...Rest >
auto remove_commas( token_string< A, B, Rest... > )
{
	if constexpr ( same_types< B, comma > )
	{
		if constexpr ( sizeof...(Rest) > 0 )
		{
			return make_token_string_t<
				A,
				decltype( remove_commas( make_token_string_t< Rest... >{} ) )
			>{};
		}
		else
		{
			return make_token_string_t< A >{};
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
			return make_token_string_t<
				decltype(
					A::from_token_string(
						remove_commas(
							parse_key_value( 
								parse_array_object(
									typename A::finalize{} 
								)
							)
						)
					)
				),
				decltype(
					parse_array_object( parse_rest{} )
				)
			>{};
		}
		else
		{
			return parse_array_object( 
				make_token_string_t<
					typename A::template append< B >,
					parse_rest
				>{}
			);
		}
	}
	else
	{
		return make_token_string_t< A, parse_all_but_a >{};
	}
}

template < typename A >
auto peel_token_string( token_string< A > ) -> A;

template < typename ...T >
auto parse( token_string< T... > tokens )
{
	using parse_result = decltype( 
		parse_array_object( tokens )
	);
	return decltype( peel_token_string( parse_result{} ) ){};
}

template < typename T >
auto parse( T t )
{
	return decltype( parse( tokenize( t ) ) ){};
}

int main( int, char ** )
{
	const auto json_string = constexpr_string(
R"json(
{
	"widget": {
		"debug": "on",
		"window": {
			"title": "Sample Konfabulator Widget",
			"name": "main_window",
			"width": 500,
			"height": 500
		},
		"image": { 
			"src": "Images/Sun.png",
			"name": "sun1",
			"hOffset": 250,
			"vOffset": 250,
			"alignment": "center"
		},
		"text": {
			"data": "Click Here",
			"size": 36,
			"style": "bold",
			"name": "text1",
			"hOffset": 250,
			"vOffset": 100,
			"alignment": "center",
			"onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
		}
	},
	"array":[],
	"array2":[3,4]
} 
)json"
	);

	auto parsed = parse( json_string );

	fwrite( parsed.to_string().to_stringview().data(), sizeof( char ), parsed.to_string().to_stringview().size(), stdout );
	
	auto widget = parse( constexpr_string( "\"widget\"" ) );
	auto text = parse( constexpr_string( "\"text\"" ) );
	auto size = parse( constexpr_string( "\"size\"" ) );
	
	return parsed[ widget ][ text ][ size ];
}
