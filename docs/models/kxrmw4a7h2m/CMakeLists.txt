cmake_minimum_required(VERSION 3.0.2)
project(kxrmw4a7h2m)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
