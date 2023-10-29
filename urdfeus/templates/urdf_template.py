urdf_template = """<?xml version="1.0" ?>
<robot name="{mesh_name}">
  <link name="link0">
    <inertial>
      <origin rpy="0 0 0" xyz="0 0 0" />
      <mass value="1.0" />
      <inertia ixx="1.00E+00" ixy="0.00E+00" ixz="0.00E+00" iyy="1.00E+00" iyz="0.00E+00" izz="1.00E+00" />
    </inertial>
    <collision>
      <origin rpy="0 0 0" xyz="0 0 0" />
      <geometry>
        <mesh filename="{collision_mesh_filepath}" scale="1 1 1" />
      </geometry>
    </collision>
    <visual>
      <origin rpy="0 0 0" xyz="0 0 0" />
      <geometry>
        <mesh filename="{visual_mesh_filepath}" scale="1 1 1" />
      </geometry>
    </visual>
  </link>
</robot>
"""  # NOQA
