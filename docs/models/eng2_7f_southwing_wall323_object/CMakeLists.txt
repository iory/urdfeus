cmake_minimum_required(VERSION 3.0.2)
project(eng2_7f_southwing_wall323_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
