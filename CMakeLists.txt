cmake_minimum_required(VERSION 3.25)

MACRO(ADD_MSVC_PRECOMPILED_HEADER Prefix PrecompiledHeader PrecompiledSource SourcesVar)
	IF(MSVC)
		GET_FILENAME_COMPONENT(PrecompiledBasename ${PrecompiledHeader} NAME_WE)
		SET(PrecompiledBinary "${CMAKE_CURRENT_BINARY_DIR}/${PrecompiledBasename}.pch")
		SET(Sources ${${SourcesVar}})

		SET_SOURCE_FILES_PROPERTIES(${Prefix}/${PrecompiledSource}
			PROPERTIES COMPILE_FLAGS "/Yc\"${PrecompiledHeader}\" /Fp\"${PrecompiledBinary}\""
			OBJECT_OUTPUTS "${PrecompiledBinary}"
		)
		SET_SOURCE_FILES_PROPERTIES(${Sources}
			PROPERTIES COMPILE_FLAGS "/Yu\"${PrecompiledHeader}\" /FI\"${PrecompiledHeader}\" /Fp\"${PrecompiledBinary}\""
			OBJECT_DEPENDS "${PrecompiledBinary}"
		)

		LIST(APPEND ${SourcesVar} ${Prefix}/${PrecompiledSource})
	ENDIF(MSVC)

	LIST(APPEND ${SourcesVar} ${Prefix}/${PrecompiledHeader})
ENDMACRO(ADD_MSVC_PRECOMPILED_HEADER)

function(assign_source_group)
	foreach(_source IN ITEMS ${ARGN})
		if (IS_ABSOLUTE "${_source}")
			file(RELATIVE_PATH _source_rel "${CMAKE_CURRENT_SOURCE_DIR}" "${_source}")
		else()
			set(_source_rel "${_source}")
		endif()
		get_filename_component(_source_path "${_source_rel}" PATH)
		string(REPLACE "/" "\\" _source_path_msvc "${_source_path}")
		source_group("${_source_path_msvc}" FILES "${_source}")
	endforeach()
endfunction(assign_source_group)


project(pgen-test VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(PROJECT_SOURCES
	src/main.cpp
	src/mylang.cpp

	src/rules/test.txt
	src/gen/test.hpp

	src/rules/mylang.txt
	src/gen/mylang.hpp
)

ADD_MSVC_PRECOMPILED_HEADER("src" "pch.hpp" "pch.cpp" PROJECT_SOURCES)

add_executable(pgen-test ${PROJECT_SOURCES})

target_include_directories(pgen-test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)

assign_source_group(${PROJECT_SOURCES})

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT pgen-test)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/libs/pgen)

ADD_DEPENDENCIES(pgen-test pgen)

add_custom_command(
	OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/src/gen/test.hpp"
	COMMAND "$<TARGET_FILE:pgen>" --output "${CMAKE_CURRENT_SOURCE_DIR}/src/gen/test.hpp" --input "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/test.txt" --namespace test
	DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/test.txt" "$<TARGET_FILE:pgen>"
	MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/test.txt"
	COMMENT "Generating ${CMAKE_CURRENT_SOURCE_DIR}/src/gen/test.hpp"
	VERBATIM
)

add_custom_command(
	OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/src/gen/mylang.hpp"
	COMMAND "$<TARGET_FILE:pgen>" --output "${CMAKE_CURRENT_SOURCE_DIR}/src/gen/mylang.hpp" --input "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/mylang.txt" --namespace mylang
	DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/mylang.txt" "$<TARGET_FILE:pgen>"
	MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/src/rules/mylang.txt"
	COMMENT "Generating ${CMAKE_CURRENT_SOURCE_DIR}/src/gen/mylang.hpp"
	VERBATIM
)
