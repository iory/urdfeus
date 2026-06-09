cmake_minimum_required(VERSION 3.0.2)
project(room602_3t_600_clean_locker_object)
find_package(catkin REQUIRED)
catkin_package()
install(DIRECTORY meshes urdf
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
