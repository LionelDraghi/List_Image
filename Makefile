# -----------------------------------------------------------------------------
# Copyright 2018 Lionel Draghi
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# -----------------------------------------------------------------------------
# This file is part of the List_Image project
# available here https://github.com/LionelDraghi/List_Image
# -----------------------------------------------------------------------------

.SILENT:

all: build check

build:
	echo --- build:
	gprbuild -P list_image.gpr
	echo

check: ./test_list_image
	echo --- tests:
	./test_list_image
	echo

.PHONY : clean
clean:
	echo --- clean:
	- gnat clean -P list_image.gpr
