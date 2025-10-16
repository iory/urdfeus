import os
from pathlib import Path
import pickle
from typing import Dict
from typing import Optional


class MeshCache:
    """Cache for processed mesh data to speed up repeated URDF conversions.

    This cache stores processed mesh data (split meshes, vertices, faces, etc.)
    at the mesh file level, not the URDF level. This allows different URDF
    configurations that share the same mesh files to reuse cached data.
    """

    def __init__(self, cache_dir: Optional[str] = None):
        """Initialize mesh cache.

        Parameters
        ----------
        cache_dir : str, optional
            Directory to store cache files. If None, uses ~/.cache/urdfeus/mesh_cache
        """
        if cache_dir is None:
            cache_dir = os.path.expanduser(
                os.path.join("~", ".cache", "urdfeus", "mesh_cache"))
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)

    def save_mesh_cache(self, mesh_cache_key: int, split_meshes: list) -> None:
        """Save split mesh cache data for a specific mesh.

        Parameters
        ----------
        mesh_cache_key : int
            Cache key computed from mesh geometry
        split_meshes : list
            List of split meshes to cache
        """
        cache_filename = f"mesh_{abs(mesh_cache_key)}.pkl"
        cache_path = self.cache_dir / cache_filename

        try:
            with open(cache_path, 'wb') as f:
                pickle.dump(split_meshes, f, protocol=pickle.HIGHEST_PROTOCOL)
        except Exception as e:
            # If saving fails, just continue without cache
            print(f"Warning: Failed to save mesh cache: {e}")

    def load_all_mesh_caches(self) -> Dict[int, list]:
        """Load all mesh caches from disk into a dictionary.

        This is useful for loading all available mesh caches at startup,
        so they can be reused across different URDF files.

        Returns
        -------
        dict
            Dictionary mapping mesh cache keys to split meshes
        """
        mesh_caches = {}

        for cache_file in self.cache_dir.glob("mesh_*.pkl"):
            try:
                # Extract cache key from filename
                cache_key_str = cache_file.stem.replace("mesh_", "")
                cache_key = int(cache_key_str)

                with open(cache_file, 'rb') as f:
                    mesh_caches[cache_key] = pickle.load(f)
            except Exception:
                # Skip corrupted cache files
                continue

        return mesh_caches

    def clear_cache(self) -> None:
        """Clear all mesh cache files."""
        for cache_file in self.cache_dir.glob("mesh_*.pkl"):
            try:
                cache_file.unlink()
            except Exception:
                pass


def get_default_cache() -> MeshCache:
    """Get default global mesh cache instance.

    Returns
    -------
    MeshCache
        Default mesh cache instance
    """
    global _default_cache
    if _default_cache is None:
        _default_cache = MeshCache()
    return _default_cache


_default_cache = None
