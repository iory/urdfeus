cmake_minimum_required(VERSION 3.0.2)
project(room73a3_external_wall_0_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
