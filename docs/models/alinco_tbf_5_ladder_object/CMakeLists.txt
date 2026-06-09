cmake_minimum_required(VERSION 3.0.2)
project(alinco_tbf_5_ladder_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
