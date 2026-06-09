cmake_minimum_required(VERSION 3.0.2)
project(room83b1_83b1_ground_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
