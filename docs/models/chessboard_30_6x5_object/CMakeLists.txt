cmake_minimum_required(VERSION 3.0.2)
project(chessboard_30_6x5_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
