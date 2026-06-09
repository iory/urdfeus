cmake_minimum_required(VERSION 3.0.2)
project(room73b2_bariera_1400_middle_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
