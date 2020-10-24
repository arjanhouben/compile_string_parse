project( compile_time_json_parse )

cmake_minimum_required( VERSION 3.15 )

set( CMAKE_CXX_STANDARD 17 )
set( CMAKE_CXX_STANDARD 20 )

add_executable( compile_time_json_parse 
	main.cpp
)

target_compile_options( compile_time_json_parse
	PRIVATE
		-Wall
		-Wextra
		-Wconversion
)