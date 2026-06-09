cmake_minimum_required(VERSION 3.0.2)
project(room73a3_plus_830_locker_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
